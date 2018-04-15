/* FluidSynth - A Software Synthesizer
 *
 * Copyright (C) 2003  Peter Hanappe and others.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */

#include "fluid_rvoice_mixer.h"
#include "fluid_rvoice.h"
#include "fluid_sys.h"
#include "fluid_rev.h"
#include "fluid_chorus.h"
#include "fluidsynth_priv.h"
#include "fluid_ladspa.h"
#include "fluid_synth.h"


// If less than x voices, the thread overhead is larger than the gain,
// so don't activate the thread(s).
#define VOICES_PER_THREAD 8

typedef struct _fluid_mixer_buffers_t fluid_mixer_buffers_t;

struct _fluid_mixer_buffers_t {
  fluid_rvoice_mixer_t* mixer; /**< Owner of object */

  fluid_rvoice_t** finished_voices; /* List of voices who have finished */
  int finished_voice_count;

  int buf_count;
  int fx_buf_count;
  
  /** buffer to store the left part of a stereo channel to.
   * Specifically a two dimensional array, containing \c buf_count buffers
   * (i.e. for each synth.audio-channels), of which each buffer contains
   * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT audio items (=samples) 
   */
  fluid_real_t* left_buf;
  
  /** dito, but for right part of a stereo channel */
  fluid_real_t* right_buf;

  /** buffer to store the left part of a stereo effects channel to.
   * Specifically a two dimensional array, containing \c fx_buf_count buffers
   * (i.e. for each synth.effects-channels), of which each buffer contains
   * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT audio items (=samples) 
   */
  fluid_real_t* fx_left_buf;
  fluid_real_t* fx_right_buf;
};

typedef struct _fluid_mixer_fx_t fluid_mixer_fx_t;

struct _fluid_mixer_fx_t {
  fluid_revmodel_t* reverb; /**< Reverb unit */
  fluid_chorus_t* chorus; /**< Chorus unit */
  int with_reverb;        /**< Should the synth use the built-in reverb unit? */
  int with_chorus;        /**< Should the synth use the built-in chorus unit? */
  int mix_fx_to_out;      /**< Should the effects be mixed in with the primary output? */
};

struct _fluid_rvoice_mixer_t {
  fluid_mixer_fx_t fx;

  int polyphony; /**< Length of voices array */
  int active_voices; /**< Number of non-null voices */

  int thread_count;
  fluid_mixer_buffers_t** buffers; /**< array of \c thread_count mixdown buffers (i.e. one for each thread). buffers[0] is the "master" buffer and always contains the latest rendered audio data. */

  fluid_rvoice_t** rvoices; /**< Voices array, sorted so that all nulls are last */
  fluid_rvoice_eventhandler_t* remove_voice_callback_userdata;
  
#ifdef LADSPA
  fluid_ladspa_fx_t* ladspa_fx; /**< Used by mixer only: Effects unit for LADSPA support. Never created or freed */
#endif
};


static void fluid_mixer_buffers_reduce(fluid_mixer_buffers_t** bufs, int begin, int end, int blockcount);
static void fluid_rvoice_mixer_buffers_mix_destination_buffers(fluid_mixer_buffers_t* dst, fluid_mixer_buffers_t* src, int current_blockcount);

static FLUID_INLINE void 
fluid_rvoice_mixer_process_fx(fluid_rvoice_mixer_t* mixer, int current_blockcount)
{
    if (mixer->fx.mix_fx_to_out)
    {
        #pragma omp single
        {
            int i;
            fluid_profile_ref_var(prof_ref);
            if (mixer->fx.with_reverb)
            {
                for (i=0; i < current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
                    fluid_revmodel_processmix(mixer->fx.reverb,
                                            &mixer->buffers[0]->fx_left_buf[SYNTH_REVERB_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                            &mixer->buffers[0]->left_buf[0 * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                            &mixer->buffers[0]->right_buf[0 * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i]);
            }
            fluid_profile(FLUID_PROF_ONE_BLOCK_REVERB, prof_ref,0,
                        current_blockcount * FLUID_BUFSIZE);
            
            if (mixer->fx.with_chorus)
            {
                for (i=0; i < current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
                    fluid_chorus_processmix(mixer->fx.chorus, 
                                            &mixer->buffers[0]->fx_left_buf[SYNTH_CHORUS_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                            &mixer->buffers[0]->left_buf[0 * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                            &mixer->buffers[0]->right_buf[0 * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i]);
            }
            fluid_profile(FLUID_PROF_ONE_BLOCK_CHORUS, prof_ref,0,
                        current_blockcount * FLUID_BUFSIZE);
        }
    }
    else
    {
        #pragma omp sections
        {
            #pragma omp section
            {
                if (mixer->fx.with_reverb)
                {
                    int i;
                    fluid_profile_ref_var(prof_ref);
                        for (i=0; i < current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
                            fluid_revmodel_processreplace(mixer->fx.reverb, 
                                                        &mixer->buffers[0]->fx_left_buf[SYNTH_REVERB_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                                        &mixer->buffers[0]->fx_left_buf[SYNTH_REVERB_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                                        &mixer->buffers[0]->fx_right_buf[SYNTH_REVERB_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i]);
                    fluid_profile(FLUID_PROF_ONE_BLOCK_REVERB, prof_ref,0,
                                current_blockcount * FLUID_BUFSIZE);
                }
            }
            
            #pragma omp section
            {
                if (mixer->fx.with_chorus)
                {
                    int i;
                    fluid_profile_ref_var(prof_ref);
                        for (i=0; i < current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
                            fluid_chorus_processreplace(mixer->fx.chorus, 
                                                        &mixer->buffers[0]->fx_left_buf[SYNTH_CHORUS_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                                        &mixer->buffers[0]->fx_left_buf[SYNTH_CHORUS_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i],
                                                        &mixer->buffers[0]->fx_right_buf[SYNTH_CHORUS_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT + i]);
                    fluid_profile(FLUID_PROF_ONE_BLOCK_CHORUS, prof_ref,0,
                                current_blockcount * FLUID_BUFSIZE);
                }
            }
        }
    }
    /* implicit omp barrier to wait for chrous and reverb before continuing with ladspa */
    
#ifdef LADSPA
  /* Run the signal through the LADSPA Fx unit. The buffers have already been
   * set up in fluid_rvoice_mixer_set_ladspa. */
  if (mixer->ladspa_fx)
  {
        #pragma omp single
        {
            fluid_ladspa_run(mixer->ladspa_fx, current_blockcount, FLUID_BUFSIZE);
            fluid_check_fpe("LADSPA");
        }
  }
#endif
}

static FLUID_INLINE void
fluid_mixer_buffers_zero(fluid_mixer_buffers_t* buffers, int current_blockcount)
{
    int size = current_blockcount * FLUID_BUFSIZE * sizeof(fluid_real_t);
  
    FLUID_MEMSET(buffers->left_buf, 0, buffers->buf_count * size);
    FLUID_MEMSET(buffers->right_buf, 0, buffers->buf_count * size);
    
    FLUID_MEMSET(buffers->fx_left_buf, 0, buffers->fx_buf_count * size);
    FLUID_MEMSET(buffers->fx_right_buf, 0, buffers->fx_buf_count * size);
}

/**
 * Glue to get fluid_rvoice_buffers_mix what it wants
 * Note: Make sure outbufs has 2 * (buf_count + fx_buf_count) elements before calling
 */
static FLUID_INLINE int 
fluid_mixer_buffers_prepare(fluid_mixer_buffers_t* buffers, fluid_real_t** outbufs)
{
  fluid_real_t *reverb_buf, *chorus_buf;
  int i;
  int with_reverb = buffers->mixer->fx.with_reverb;
  int with_chorus = buffers->mixer->fx.with_chorus;

  /* Set up the reverb and chorus buffers only when the effect is enabled or
   * when LADSPA is active. Nonexisting buffers are detected in the DSP loop.
   * Not sending the effect signals saves some time in that case. */
#ifdef LADSPA
  int with_ladspa = (buffers->mixer->ladspa_fx != NULL);
  with_reverb = (with_reverb | with_ladspa);
  with_chorus = (with_chorus | with_ladspa);
#endif
  reverb_buf = (with_reverb) ? &buffers->fx_left_buf[SYNTH_REVERB_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT] : NULL;
  chorus_buf = (with_chorus) ? &buffers->fx_left_buf[SYNTH_CHORUS_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT] : NULL;
  outbufs[buffers->buf_count*2 + SYNTH_REVERB_CHANNEL] = reverb_buf;
  outbufs[buffers->buf_count*2 + SYNTH_CHORUS_CHANNEL] = chorus_buf;

      /* The output associated with a MIDI channel is wrapped around
       * using the number of audio groups as modulo divider.  This is
       * typically the number of output channels on the 'sound card',
       * as long as the LADSPA Fx unit is not used. In case of LADSPA
       * unit, think of it as subgroups on a mixer.
       *
       * For example: Assume that the number of groups is set to 2.
       * Then MIDI channel 1, 3, 5, 7 etc. go to output 1, channels 2,
       * 4, 6, 8 etc to output 2.  Or assume 3 groups: Then MIDI
       * channels 1, 4, 7, 10 etc go to output 1; 2, 5, 8, 11 etc to
       * output 2, 3, 6, 9, 12 etc to output 3.
       */

  for (i = 0; i < buffers->buf_count; i++) {
    outbufs[i*2] = &buffers->left_buf[i * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT];
    outbufs[i*2+1] = &buffers->right_buf[i * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT];
  }
  return buffers->buf_count*2 + 2;
}


static FLUID_INLINE void
fluid_finish_rvoice(fluid_mixer_buffers_t* buffers, fluid_rvoice_t* rvoice)
{
  if (buffers->finished_voice_count < buffers->mixer->polyphony)
    buffers->finished_voices[buffers->finished_voice_count++] = rvoice;
  else
    FLUID_LOG(FLUID_ERR, "Exceeded finished voices array, try increasing polyphony");
}

static void   
fluid_mixer_buffer_process_finished_voices(fluid_mixer_buffers_t* buffers)
{
  int i,j;
  for (i=0; i < buffers->finished_voice_count; i++) {
    fluid_rvoice_t* v = buffers->finished_voices[i];
    int* av = &buffers->mixer->active_voices; 
    for (j=0; j < *av; j++) {
      if (v == buffers->mixer->rvoices[j]) {
        (*av)--;
        /* Pack the array */
        if (j < *av) 
          buffers->mixer->rvoices[j] = buffers->mixer->rvoices[*av];
      }
    }
    
    fluid_rvoice_eventhandler_finished_voice_callback(buffers->mixer->remove_voice_callback_userdata, v);
  }
  buffers->finished_voice_count = 0;
}

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_add_voice)
{
  int i;
  fluid_rvoice_mixer_t* mixer = obj;
  fluid_rvoice_t* voice = param[0].ptr;

  if (mixer->active_voices < mixer->polyphony) {
    mixer->rvoices[mixer->active_voices++] = voice;
    return; // success
  }
  
  /* See if any voices just finished, if so, take its place.
     This can happen in voice overflow conditions. */
  for (i=0; i < mixer->active_voices; i++) {
    if (mixer->rvoices[i] == voice) {
      FLUID_LOG(FLUID_ERR, "Internal error: Trying to replace an existing rvoice in fluid_rvoice_mixer_add_voice?!");
      return;
    }
    if (mixer->rvoices[i]->envlfo.volenv.section == FLUID_VOICE_ENVFINISHED) {
      fluid_finish_rvoice(mixer->buffers[0], mixer->rvoices[i]);
      mixer->rvoices[i] = voice;
      return; // success
    }
  }

  /* This should never happen */
  FLUID_LOG(FLUID_ERR, "Trying to exceed polyphony in fluid_rvoice_mixer_add_voice");
  return;
}


static int fluid_rvoice_mixer_set_polyphony_LOCAL(fluid_rvoice_mixer_t* mixer, int polyphony)
{
  void* newptr;
  int i;
  
  if (mixer->active_voices > polyphony || mixer->buffers[0]->finished_voice_count > polyphony) 
    return FLUID_FAILED;
  
  newptr = FLUID_REALLOC(mixer->rvoices, polyphony * sizeof(fluid_rvoice_t*));
  if (newptr == NULL)
  {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        return FLUID_FAILED;
  }
  mixer->rvoices = newptr;

  for(i=0; i < mixer->thread_count; i++)
  {
    newptr = FLUID_REALLOC(mixer->buffers[i]->finished_voices, polyphony * sizeof(fluid_rvoice_t*));
    if (newptr == NULL)
    {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        return FLUID_FAILED;
    }
    
    mixer->buffers[i]->finished_voices = newptr;
  }

  mixer->polyphony = polyphony;
  return FLUID_OK;
}

/**
 * Update polyphony - max number of voices (NOTE: not hard real-time capable)
 * @return FLUID_OK or FLUID_FAILED
 */
DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_polyphony)
{
  fluid_rvoice_mixer_t* handler = obj;
  int value = param[0].i;
  
  fluid_rvoice_mixer_set_polyphony_LOCAL(handler, value);
}

/**
 * Mix mono voice \c dsp_buf down to \c dest_bufs
 *
 * @param buffers Destination buffer(s)
 * @param dsp_buf Mono sample source
 * @param samplecount Number of samples to process (no FLUID_BUFSIZE restriction)
 * @param dest_bufs Array of buffers to mixdown to
 * @param dest_bufcount Length of dest_bufs
 */
static void 
fluid_rvoice_buffers_mix(fluid_rvoice_buffers_t* buffers, 
                         fluid_real_t *FLUID_RESTRICT dsp_buf, int samplecount, 
                         fluid_real_t** dest_bufs, int dest_bufcount)
{
    int bufcount = buffers->count;
    int i, j, dsp_i;
    
    if (!samplecount || !bufcount || !dest_bufcount) 
        return;

    for (i=0; i < bufcount; i++)
    {
        fluid_real_t *FLUID_RESTRICT buf;
        fluid_real_t amp = buffers->bufs[i].amp;
        j = buffers->bufs[i].mapping;
        
        if (FLUID_UNLIKELY(0 > j || j >= dest_bufcount))
        {
            FLUID_LOG(FLUID_ERR, "THIS SHOULD NEVER HAPPEN: rvoice mapping out of range");
            continue;
        }
        
        buf = dest_bufs[j];
        if(amp == 0.0f || buf == NULL)
        {
            continue;
        }
        
        for (dsp_i = 0; dsp_i < samplecount; dsp_i++)
        {
            buf[dsp_i] += amp * dsp_buf[dsp_i];
        }
    }
}

static void fluid_mixer_buffers_reduce(fluid_mixer_buffers_t** bufs, int begin, int end, int blockcount)
{
    if (end - begin == 1)
    {
        return;
    }
    else
    {
        int pivot = (begin + end) / 2;
        #pragma omp task
        fluid_mixer_buffers_reduce(bufs, begin, pivot, blockcount);
        #pragma omp task
        fluid_mixer_buffers_reduce(bufs, pivot, end, blockcount);

        #pragma omp taskwait
        fluid_rvoice_mixer_buffers_mix_destination_buffers(bufs[begin], bufs[pivot], blockcount);
    }
}

static void fluid_rvoice_mixer_buffers_mix_destination_buffers(fluid_mixer_buffers_t* dst, fluid_mixer_buffers_t* src, int current_blockcount)
{
    int i;
    
    for (i=0; i < src->buf_count * current_blockcount * FLUID_BUFSIZE; i++)
    {
        dst->left_buf[i] += src->left_buf[i];
    }
    
    for (i=0; i < src->buf_count * current_blockcount * FLUID_BUFSIZE; i++)
    {
        dst->right_buf[i] += src->right_buf[i];
    }
    
    for (i=0; i < src->buf_count * current_blockcount * FLUID_BUFSIZE; i++)
    {
        dst->fx_left_buf[i] += src->fx_left_buf[i];
    }
    
    for (i=0; i < src->buf_count * current_blockcount * FLUID_BUFSIZE; i++)
    {
        dst->fx_right_buf[i] += src->fx_right_buf[i];
    }
    
    if(FLUID_LIKELY(dst->mixer->polyphony > dst->finished_voice_count + src->finished_voice_count))
    {
        FLUID_MEMCPY(&dst->finished_voices[dst->finished_voice_count], &src->finished_voices[0], sizeof(*src->finished_voices) * src->finished_voice_count);
        dst->finished_voice_count += src->finished_voice_count;
        src->finished_voice_count = 0;
    }
    else
    {
        FLUID_LOG(FLUID_ERR, "THIS SHOULD NEVER HAPPEN: Exceeding finished voices array in fluid_rvoice_mixer_buffers_mix_destination_buffers(), unable to recover, voices leaked.");
    }
}

static int 
fluid_mixer_buffers_init(fluid_rvoice_mixer_t* mixer, int thread_count, int buf_count, int fx_buf_count)
{
    const int samplecount = FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT;
    int ret = FLUID_FAILED;
    
    mixer->thread_count = thread_count;
    mixer->buffers = FLUID_ARRAY(fluid_mixer_buffers_t*, thread_count);
    if (mixer->buffers == NULL)
    {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        return ret;
    }
    
    /**
     * Allocate the mixdown buffers for each thread below. Do this in a parallel region to achieve NUMA-aware
     * memory allocation.
     */
    #pragma omp parallel num_threads(thread_count) shared(ret)
    {
        int i
#if HAVE_OPENMP
        = omp_get_thread_num();
#else
        = 0;
#endif
        
        mixer->buffers[i] = FLUID_NEW(fluid_mixer_buffers_t);
        if(mixer->buffers[i] == NULL)
        {
            #pragma omp critical
            ret = FLUID_FAILED;
        }
        else
        {
            /* init all all elements and all its members to zero, makes error recorvery easier */
            FLUID_MEMSET(mixer->buffers[i], 0, sizeof(*mixer->buffers[i]));
            
            mixer->buffers[i]->mixer = mixer;
            mixer->buffers[i]->buf_count = buf_count;
            mixer->buffers[i]->fx_buf_count = fx_buf_count;
            
            if ((mixer->buffers[i]->left_buf = FLUID_ARRAY(fluid_real_t, buf_count * samplecount)) == NULL ||
                (mixer->buffers[i]->right_buf = FLUID_ARRAY(fluid_real_t, buf_count * samplecount)) == NULL ||
                (mixer->buffers[i]->fx_left_buf = FLUID_ARRAY(fluid_real_t, fx_buf_count * samplecount)) == NULL ||
                (mixer->buffers[i]->fx_right_buf = FLUID_ARRAY(fluid_real_t, fx_buf_count * samplecount)) == NULL)
            {
                #pragma omp critical
                {
                    FLUID_LOG(FLUID_ERR, "Out of memory");
                    ret = FLUID_FAILED;
                }
            }
            else
            {
                FLUID_MEMSET(mixer->buffers[i]->left_buf, 0, buf_count * samplecount * sizeof(*mixer->buffers[i]->left_buf));
                FLUID_MEMSET(mixer->buffers[i]->right_buf, 0, buf_count * samplecount * sizeof(*mixer->buffers[i]->right_buf));
                FLUID_MEMSET(mixer->buffers[i]->fx_left_buf, 0, fx_buf_count * samplecount * sizeof(*mixer->buffers[i]->fx_left_buf));
                FLUID_MEMSET(mixer->buffers[i]->fx_right_buf, 0, fx_buf_count * samplecount * sizeof(*mixer->buffers[i]->fx_right_buf));
            }
        }
    }
    
    ret = FLUID_OK;
    return ret;
}

/**
 * Note: Not hard real-time capable (calls malloc)
 */
DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_samplerate)
{
  fluid_rvoice_mixer_t* mixer = obj;
  fluid_real_t samplerate = param[1].real; // because fluid_synth_update_mixer() puts real into arg2
  
  if (mixer->fx.chorus)
    delete_fluid_chorus(mixer->fx.chorus);
  
  mixer->fx.chorus = new_fluid_chorus(samplerate);
  if (mixer->fx.reverb)
	  fluid_revmodel_samplerate_change(mixer->fx.reverb, samplerate);
  
#if LADSPA
  if (mixer->ladspa_fx != NULL)
  {
    fluid_ladspa_set_sample_rate(mixer->ladspa_fx, samplerate);
  }
#endif
}


/**
 * @param buf_count number of primary stereo buffers
 * @param fx_buf_count number of stereo effect buffers
 */
fluid_rvoice_mixer_t* 
new_fluid_rvoice_mixer(int thread_count, int buf_count, int fx_buf_count, fluid_real_t sample_rate, int polyphony, void* userdata)
{
    fluid_rvoice_mixer_t* mixer = FLUID_NEW(fluid_rvoice_mixer_t);
    if (mixer == NULL) {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        return NULL;
    }
    FLUID_MEMSET(mixer, 0, sizeof(fluid_rvoice_mixer_t));
        
    if (fluid_mixer_buffers_init(mixer, thread_count, buf_count, fx_buf_count) == FLUID_FAILED)
    {
        goto error_rec;
    }
    
    if (fluid_rvoice_mixer_set_polyphony_LOCAL(mixer, polyphony) == FLUID_FAILED)
    {
        goto error_rec;
    }
    
    /* allocate the reverb module */
    mixer->fx.reverb = new_fluid_revmodel(sample_rate);
    mixer->fx.chorus = new_fluid_chorus(sample_rate);
    if (mixer->fx.reverb == NULL || mixer->fx.chorus == NULL)
    {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        goto error_rec;
    }
    
    mixer->remove_voice_callback_userdata = userdata;
    
    return mixer;
  
error_rec:
    delete_fluid_rvoice_mixer(mixer);
    return NULL;
    
}

void delete_fluid_rvoice_mixer(fluid_rvoice_mixer_t* mixer)
{
    int i;
    fluid_return_if_fail(mixer != NULL);
  
    for(i=0; i < mixer->thread_count; i++)
    {
        if(mixer->buffers != NULL && mixer->buffers[i] != NULL)
        {
            FLUID_FREE(mixer->buffers[i]->finished_voices);
            FLUID_FREE(mixer->buffers[i]->left_buf);
            FLUID_FREE(mixer->buffers[i]->right_buf);
            FLUID_FREE(mixer->buffers[i]->fx_left_buf);
            FLUID_FREE(mixer->buffers[i]->fx_right_buf);
        }
    }
    
    delete_fluid_revmodel(mixer->fx.reverb);
    delete_fluid_chorus(mixer->fx.chorus);
    
    FLUID_FREE(mixer->rvoices);
    FLUID_FREE(mixer);
}


#ifdef LADSPA				    
/**
 * Set a LADSPS fx instance to be used by the mixer and assign the mixer buffers
 * as LADSPA host buffers with sensible names */
void fluid_rvoice_mixer_set_ladspa(fluid_rvoice_mixer_t* mixer,
        fluid_ladspa_fx_t *ladspa_fx, int audio_groups)
{
    mixer->ladspa_fx = ladspa_fx;
    if (ladspa_fx == NULL)
    {
        return;
    }

    fluid_ladspa_add_host_ports(ladspa_fx, "Main:L", audio_groups,
            mixer->buffers[0]->left_buf, FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT);

    fluid_ladspa_add_host_ports(ladspa_fx, "Main:R", audio_groups,
            mixer->buffers[0]->right_buf, FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT);

    fluid_ladspa_add_host_ports(ladspa_fx, "Reverb:Send", 1,
            &mixer->buffers[0]->fx_left_buf[SYNTH_REVERB_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT],
            FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT);

    fluid_ladspa_add_host_ports(ladspa_fx, "Chorus:Send", 1,
            &mixer->buffers[0]->fx_left_buf[SYNTH_CHORUS_CHANNEL * FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT],
            FLUID_BUFSIZE * FLUID_MIXER_MAX_BUFFERS_DEFAULT);
}
#endif

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_reverb_enabled)
{
  fluid_rvoice_mixer_t* mixer = obj;
  int on = param[0].i;
  
  mixer->fx.with_reverb = on;
}

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_chorus_enabled)
{
  fluid_rvoice_mixer_t* mixer = obj;
  int on = param[0].i;
  mixer->fx.with_chorus = on;
}

void fluid_rvoice_mixer_set_mix_fx(fluid_rvoice_mixer_t* mixer, int on)
{
  mixer->fx.mix_fx_to_out = on;
}

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_chorus_params)
{
  fluid_rvoice_mixer_t* mixer = obj;
  int set = param[0].i;
  int nr = param[1].i;
  fluid_real_t level = param[2].real;
  fluid_real_t speed = param[3].real;
  fluid_real_t depth_ms = param[4].real;
  int type = param[5].i;
  
  fluid_chorus_set(mixer->fx.chorus, set, nr, level, speed, depth_ms, type);
}

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_reverb_params)
{
    fluid_rvoice_mixer_t* mixer = obj;
    int set = param[0].i;
    fluid_real_t roomsize = param[1].real;
    fluid_real_t damping = param[2].real;
    fluid_real_t width = param[3].real;
    fluid_real_t level = param[4].real;
    
  fluid_revmodel_set(mixer->fx.reverb, set, roomsize, damping, width, level); 
}

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_reset_reverb)
{
  fluid_rvoice_mixer_t* mixer = obj;
  fluid_revmodel_reset(mixer->fx.reverb);
}

DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_reset_chorus)
{
  fluid_rvoice_mixer_t* mixer = obj;
  fluid_chorus_reset(mixer->fx.chorus);
}

int fluid_rvoice_mixer_get_bufs(fluid_rvoice_mixer_t* mixer, 
				  fluid_real_t** left, fluid_real_t** right)
{
  *left = mixer->buffers[0]->left_buf;
  *right = mixer->buffers[0]->right_buf;
  return mixer->buffers[0]->buf_count;
}

int fluid_rvoice_mixer_get_fx_bufs(fluid_rvoice_mixer_t* mixer, 
                  fluid_real_t** fx_left, fluid_real_t** fx_right)
{
  *fx_left = mixer->buffers[0]->fx_left_buf;
  *fx_right = mixer->buffers[0]->fx_right_buf;
  return mixer->buffers[0]->fx_buf_count;
}

int fluid_rvoice_mixer_get_bufcount(fluid_rvoice_mixer_t* mixer)
{
    return FLUID_MIXER_MAX_BUFFERS_DEFAULT;
}

#if WITH_PROFILING
int fluid_rvoice_mixer_get_active_voices(fluid_rvoice_mixer_t* mixer)
{
	return mixer->active_voices;
}
#endif


/**
 * Synthesize audio into buffers
 * @param current_blockcount number of blocks to render, each having FLUID_BUFSIZE samples 
 * @return number of blocks rendered
 */
int 
fluid_rvoice_mixer_render(fluid_rvoice_mixer_t* mixer, int current_blockcount)
{
    const int active_voice_count = mixer->active_voices;
    const int thread_count = active_voice_count >= mixer->thread_count ? mixer->thread_count : 1;
    fluid_profile_ref_var(prof_ref_one_block);
    fluid_profile_ref_var(prof_ref);
    
    
    /* cannot render more blocks than allocated */
    current_blockcount = current_blockcount > FLUID_MIXER_MAX_BUFFERS_DEFAULT
                        ? FLUID_MIXER_MAX_BUFFERS_DEFAULT : current_blockcount;

    fluid_profile(FLUID_PROF_ONE_BLOCK_CLEAR, prof_ref_one_block, mixer->active_voices,
                    current_blockcount * FLUID_BUFSIZE);
    
    
    #pragma omp parallel default(shared) num_threads(thread_count) firstprivate(mixer, active_voice_count, current_blockcount)
    {
        fluid_mixer_buffers_t *my_local_buffer
#if HAVE_OPENMP
         = mixer->buffers[omp_get_thread_num()];
#else
         = mixer->buffers[0];        
#endif
        FLUID_DECLARE_VLA(fluid_real_t*, dest_bufs, my_local_buffer->buf_count * 2 + my_local_buffer->fx_buf_count * 2);
        const int dest_bufcount = fluid_mixer_buffers_prepare(my_local_buffer, dest_bufs);
        
        /* temporary mono voice render buffer */
        FLUID_DECLARE_VLA(fluid_real_t, local_buf, FLUID_BUFSIZE*current_blockcount);
        int i;
        
        /* make each thread zero its local mixdown buffer */
        fluid_mixer_buffers_zero(my_local_buffer, current_blockcount);
        
        #pragma omp for schedule(dynamic)
        for (i=0; i < active_voice_count; i++)
        {
            fluid_rvoice_t* rvoice = mixer->rvoices[i];
            
            int j, samples = 0;
            
            /**
             * Synthesize one voice and add to buffer.
             * @note If return value is less than blockcount*FLUID_BUFSIZE, that means 
             * voice has been finished
             */
            for (j=0; j < current_blockcount; j++)
            {
                int s = fluid_rvoice_write(rvoice, &local_buf[FLUID_BUFSIZE*j]);
                
                if (s == -1)
                {
                    s = FLUID_BUFSIZE; /* Voice is quiet, TODO: optimize away memset/mix */
                    FLUID_MEMSET(&local_buf[FLUID_BUFSIZE*j], 0, FLUID_BUFSIZE*sizeof(fluid_real_t));
                }
                samples += s;
                if (s < FLUID_BUFSIZE)
                {
                    break;
                }
            }
            
            /* mixdown the processed voice to the threads local mixdown buffer */
            fluid_rvoice_buffers_mix(&rvoice->buffers, local_buf, samples, dest_bufs, dest_bufcount);
            
            
            if (samples < current_blockcount * FLUID_BUFSIZE)
            {
                fluid_finish_rvoice(my_local_buffer, rvoice);
            }
        
    #if WITH_PROFILING
            #pragma omp critical
            fluid_profile(FLUID_PROF_ONE_BLOCK_VOICE, prof_ref, 1, current_blockcount * FLUID_BUFSIZE);
    #endif
        }
        /*** implicit omp barrier ***/
        
        #pragma omp single
        {
            fluid_mixer_buffers_reduce(mixer->buffers, 0, thread_count, current_blockcount);
            
            fluid_profile(FLUID_PROF_ONE_BLOCK_VOICES, prof_ref_one_block, mixer->active_voices,
                            current_blockcount * FLUID_BUFSIZE);
        }
        
        // Process reverb & chorus
        fluid_rvoice_mixer_process_fx(mixer, current_blockcount);
    }
    
    // Call the callback and pack active voice array
    fluid_mixer_buffer_process_finished_voices(mixer->buffers[0]);

  return current_blockcount;
}
