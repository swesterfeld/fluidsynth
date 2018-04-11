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

  int buf_blocks;             /**< Number of blocks allocated in the buffers */

  int buf_count;
  fluid_real_t** left_buf;
  fluid_real_t** right_buf;

  int fx_buf_count;
  fluid_real_t** fx_left_buf;
  fluid_real_t** fx_right_buf;
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

  fluid_mixer_buffers_t buffers; /**< Used by mixer only: own buffers */
  void (*remove_voice_callback)(void*, fluid_rvoice_t*); /**< Used by mixer only: Receive this callback every time a voice is removed */
  void* remove_voice_callback_userdata;

  fluid_rvoice_t** rvoices; /**< Read-only: Voices array, sorted so that all nulls are last */
  int polyphony; /**< Read-only: Length of voices array */
  int active_voices; /**< Read-only: Number of non-null voices */
  int current_blockcount;      /**< Read-only: how many blocks to process this time */

#ifdef LADSPA
  fluid_ladspa_fx_t* ladspa_fx; /**< Used by mixer only: Effects unit for LADSPA support. Never created or freed */
#endif
};

static FLUID_INLINE void 
fluid_rvoice_mixer_process_fx(fluid_rvoice_mixer_t* mixer)
{
  int i;
  fluid_profile_ref_var(prof_ref);
  if (mixer->fx.with_reverb) {
    if (mixer->fx.mix_fx_to_out) {
      for (i=0; i < mixer->current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
        fluid_revmodel_processmix(mixer->fx.reverb, 
                                  &mixer->buffers.fx_left_buf[SYNTH_REVERB_CHANNEL][i],
				  &mixer->buffers.left_buf[0][i],
				  &mixer->buffers.right_buf[0][i]);
    } 
    else {
      for (i=0; i < mixer->current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
        fluid_revmodel_processreplace(mixer->fx.reverb, 
                                  &mixer->buffers.fx_left_buf[SYNTH_REVERB_CHANNEL][i],
				  &mixer->buffers.fx_left_buf[SYNTH_REVERB_CHANNEL][i],
				  &mixer->buffers.fx_right_buf[SYNTH_REVERB_CHANNEL][i]);
    }
    fluid_profile(FLUID_PROF_ONE_BLOCK_REVERB, prof_ref,0,
	              mixer->current_blockcount * FLUID_BUFSIZE);
  }
  
  if (mixer->fx.with_chorus) {
    if (mixer->fx.mix_fx_to_out) {
      for (i=0; i < mixer->current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
        fluid_chorus_processmix(mixer->fx.chorus, 
                                &mixer->buffers.fx_left_buf[SYNTH_CHORUS_CHANNEL][i],
			        &mixer->buffers.left_buf[0][i],
				&mixer->buffers.right_buf[0][i]);
    } 
    else {
      for (i=0; i < mixer->current_blockcount * FLUID_BUFSIZE; i += FLUID_BUFSIZE)
        fluid_chorus_processreplace(mixer->fx.chorus, 
                                &mixer->buffers.fx_left_buf[SYNTH_CHORUS_CHANNEL][i],
				&mixer->buffers.fx_left_buf[SYNTH_CHORUS_CHANNEL][i],
				&mixer->buffers.fx_right_buf[SYNTH_CHORUS_CHANNEL][i]);
    }
    fluid_profile(FLUID_PROF_ONE_BLOCK_CHORUS, prof_ref,0,
	              mixer->current_blockcount * FLUID_BUFSIZE);
  }
  
#ifdef LADSPA
  /* Run the signal through the LADSPA Fx unit. The buffers have already been
   * set up in fluid_rvoice_mixer_set_ladspa. */
  if (mixer->ladspa_fx) {
      fluid_ladspa_run(mixer->ladspa_fx, mixer->current_blockcount, FLUID_BUFSIZE);
      fluid_check_fpe("LADSPA");
  }
#endif
}

/**
 * During rendering, rvoices might be finished. Set this callback
 * for getting a callback any time the rvoice is finished.
 */
void fluid_rvoice_mixer_set_finished_voices_callback(
  fluid_rvoice_mixer_t* mixer,
  void (*func)(void*, fluid_rvoice_t*),
  void* userdata)
{
  mixer->remove_voice_callback_userdata = userdata;
  mixer->remove_voice_callback = func;
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
  reverb_buf = (with_reverb) ? buffers->fx_left_buf[SYNTH_REVERB_CHANNEL] : NULL;
  chorus_buf = (with_chorus) ? buffers->fx_left_buf[SYNTH_CHORUS_CHANNEL] : NULL;
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
    outbufs[i*2] = buffers->left_buf[i];
    outbufs[i*2+1] = buffers->right_buf[i];
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
    if (buffers->mixer->remove_voice_callback)
      buffers->mixer->remove_voice_callback(
        buffers->mixer->remove_voice_callback_userdata, v);
  }
  buffers->finished_voice_count = 0;
}

static FLUID_INLINE void fluid_rvoice_mixer_process_finished_voices(fluid_rvoice_mixer_t* mixer)
{
  fluid_mixer_buffer_process_finished_voices(&mixer->buffers);
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
      fluid_finish_rvoice(&mixer->buffers, mixer->rvoices[i]);
      mixer->rvoices[i] = voice;
      return; // success
    }
  }

  /* This should never happen */
  FLUID_LOG(FLUID_ERR, "Trying to exceed polyphony in fluid_rvoice_mixer_add_voice");
  return;
}

static int 
fluid_mixer_buffers_update_polyphony(fluid_mixer_buffers_t* buffers, int value)
{
  void* newptr;

  if (buffers->finished_voice_count > value) 
    return FLUID_FAILED;
  
  newptr = FLUID_REALLOC(buffers->finished_voices, value * sizeof(fluid_rvoice_t*));
  if (newptr == NULL && value > 0) 
    return FLUID_FAILED;
  buffers->finished_voices = newptr;  
  return FLUID_OK;
}

/**
 * Update polyphony - max number of voices (NOTE: not hard real-time capable)
 * @return FLUID_OK or FLUID_FAILED
 */
DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_polyphony)
{
  void* newptr;
  fluid_rvoice_mixer_t* handler = obj;
  int value = param[0].i;
  
  if (handler->active_voices > value) 
    return /*FLUID_FAILED*/;

  newptr = FLUID_REALLOC(handler->rvoices, value * sizeof(fluid_rvoice_t*));
  if (newptr == NULL) 
    return /*FLUID_FAILED*/;
  handler->rvoices = newptr;

  if (fluid_mixer_buffers_update_polyphony(&handler->buffers, value) 
      == FLUID_FAILED)
    return /*FLUID_FAILED*/;

  handler->polyphony = value;
  return /*FLUID_OK*/;
}


static void 
fluid_render_loop_singlethread(fluid_rvoice_mixer_t* mixer)
{
    const int active_voice_count = mixer->active_voices;
    const int thread_count = active_voice_count >= VOICES_PER_THREAD ? : 4 : 1/*mixer->thread_count*/;
    int i;
    fluid_profile_ref_var(prof_ref);
    
    #pragma omp parallel for schedule(static) default(none) num_threads(thread_count) private(i) firstprivate(mixer, active_voice_count)
    for (i=0; i < active_voice_count; i++)
    {
        fluid_rvoice_t* rvoice = mixer->rvoices[i];
        
        int j, samples = 0;
        FLUID_DECLARE_VLA(fluid_real_t, local_buf, FLUID_BUFSIZE*mixer->current_blockcount);
        
        /**
        * Synthesize one voice and add to buffer.
        * NOTE: If return value is less than blockcount*FLUID_BUFSIZE, that means 
        * voice has been finished, removed and possibly replaced with another voice.
        * @return Number of samples written 
        */
        for (j=0; j < mixer->current_blockcount; j++)
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
        
        FLUID_DECLARE_VLA(fluid_real_t*, bufs, mixer->buffers.buf_count * 2 + mixer->buffers.fx_buf_count * 2);
        int bufcount = fluid_mixer_buffers_prepare(&mixer->buffers, bufs);
        fluid_rvoice_buffers_mix(&rvoice->buffers, local_buf, samples, bufs, bufcount);
        
        
        if (samples < mixer->current_blockcount * FLUID_BUFSIZE)
        {
            fluid_finish_rvoice(&mixer->buffers, rvoice);
            
        }
       
#if WITH_PROFILING
        #pragma omp critical
        fluid_profile(FLUID_PROF_ONE_BLOCK_VOICE, prof_ref, 1, mixer->current_blockcount * FLUID_BUFSIZE);
#endif
    }
    
}


static FLUID_INLINE void
fluid_mixer_buffers_zero(fluid_mixer_buffers_t* buffers)
{
  int i;
  int size = buffers->mixer->current_blockcount * FLUID_BUFSIZE * sizeof(fluid_real_t);
  /* TODO: Optimize by only zero out the buffers we actually use later on. */
  for (i=0; i < buffers->buf_count; i++) {
    FLUID_MEMSET(buffers->left_buf[i], 0, size);
    FLUID_MEMSET(buffers->right_buf[i], 0, size);
  }
  for (i=0; i < buffers->fx_buf_count; i++) {
    FLUID_MEMSET(buffers->fx_left_buf[i], 0, size);
    FLUID_MEMSET(buffers->fx_right_buf[i], 0, size);
  }
}



static int 
fluid_mixer_buffers_init(fluid_mixer_buffers_t* buffers, fluid_rvoice_mixer_t* mixer)
{
  int i, samplecount;
  
  buffers->mixer = mixer;
  buffers->buf_count = buffers->mixer->buffers.buf_count;
  buffers->fx_buf_count = buffers->mixer->buffers.fx_buf_count;
  buffers->buf_blocks = buffers->mixer->buffers.buf_blocks;
  samplecount = FLUID_BUFSIZE * buffers->buf_blocks;
  
 
  /* Left and right audio buffers */

  buffers->left_buf = FLUID_ARRAY(fluid_real_t*, buffers->buf_count);
  buffers->right_buf = FLUID_ARRAY(fluid_real_t*, buffers->buf_count);

  if ((buffers->left_buf == NULL) || (buffers->right_buf == NULL)) {
    FLUID_LOG(FLUID_ERR, "Out of memory");
    return 0;
  }

  FLUID_MEMSET(buffers->left_buf, 0, buffers->buf_count * sizeof(fluid_real_t*));
  FLUID_MEMSET(buffers->right_buf, 0, buffers->buf_count * sizeof(fluid_real_t*));

  for (i = 0; i < buffers->buf_count; i++) {

    buffers->left_buf[i] = FLUID_ARRAY(fluid_real_t, samplecount);
    buffers->right_buf[i] = FLUID_ARRAY(fluid_real_t, samplecount);

    if ((buffers->left_buf[i] == NULL) || (buffers->right_buf[i] == NULL)) {
      FLUID_LOG(FLUID_ERR, "Out of memory");
      return 0;
    }
  }

  /* Effects audio buffers */

  buffers->fx_left_buf = FLUID_ARRAY(fluid_real_t*, buffers->fx_buf_count);
  buffers->fx_right_buf = FLUID_ARRAY(fluid_real_t*, buffers->fx_buf_count);

  if ((buffers->fx_left_buf == NULL) || (buffers->fx_right_buf == NULL)) {
    FLUID_LOG(FLUID_ERR, "Out of memory");
    return 0;
  }

  FLUID_MEMSET(buffers->fx_left_buf, 0, buffers->fx_buf_count * sizeof(fluid_real_t*));
  FLUID_MEMSET(buffers->fx_right_buf, 0, buffers->fx_buf_count * sizeof(fluid_real_t*));

  for (i = 0; i < buffers->fx_buf_count; i++) {
    buffers->fx_left_buf[i] = FLUID_ARRAY(fluid_real_t, samplecount);
    buffers->fx_right_buf[i] = FLUID_ARRAY(fluid_real_t, samplecount);

    if ((buffers->fx_left_buf[i] == NULL) || (buffers->fx_right_buf[i] == NULL)) {
      FLUID_LOG(FLUID_ERR, "Out of memory");
      return 0;
    }
  }
  
  buffers->finished_voices = NULL;
  if (fluid_mixer_buffers_update_polyphony(buffers, mixer->polyphony) 
      == FLUID_FAILED) {
    FLUID_LOG(FLUID_ERR, "Out of memory");
    return 0;
  }
  
  return 1;
}

/**
 * Note: Not hard real-time capable (calls malloc)
 */
DECLARE_FLUID_RVOICE_FUNCTION(fluid_rvoice_mixer_set_samplerate)
{
  int i;
  fluid_rvoice_mixer_t* mixer = obj;
  fluid_real_t samplerate = param[1].real;
  param[0].real = param[1].real; // FIXME: this is needed for fluid_rvoice_set_output_rate()
  
  if (mixer->fx.chorus)
    delete_fluid_chorus(mixer->fx.chorus);
  mixer->fx.chorus = new_fluid_chorus(samplerate);
  if (mixer->fx.reverb)
	  fluid_revmodel_samplerate_change(mixer->fx.reverb, samplerate);
  for (i=0; i < mixer->active_voices; i++)
    fluid_rvoice_set_output_rate(mixer->rvoices[i], param);
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
new_fluid_rvoice_mixer(int buf_count, int fx_buf_count, fluid_real_t sample_rate)
{
  fluid_rvoice_mixer_t* mixer = FLUID_NEW(fluid_rvoice_mixer_t);
  if (mixer == NULL) {
    FLUID_LOG(FLUID_ERR, "Out of memory");
    return NULL;
  }
  FLUID_MEMSET(mixer, 0, sizeof(fluid_rvoice_mixer_t));
  mixer->buffers.buf_count = buf_count;
  mixer->buffers.fx_buf_count = fx_buf_count;
  mixer->buffers.buf_blocks = FLUID_MIXER_MAX_BUFFERS_DEFAULT;
  
  /* allocate the reverb module */
  mixer->fx.reverb = new_fluid_revmodel(sample_rate);
  mixer->fx.chorus = new_fluid_chorus(sample_rate);
  if (mixer->fx.reverb == NULL || mixer->fx.chorus == NULL) {
    FLUID_LOG(FLUID_ERR, "Out of memory");
    delete_fluid_rvoice_mixer(mixer);
    return NULL;
  }
  
  if (!fluid_mixer_buffers_init(&mixer->buffers, mixer)) {
    delete_fluid_rvoice_mixer(mixer);
    return NULL;
  }
  
  return mixer;
}

static void
fluid_mixer_buffers_free(fluid_mixer_buffers_t* buffers)
{
  int i;
  
  FLUID_FREE(buffers->finished_voices);
  
  /* free all the sample buffers */
  if (buffers->left_buf != NULL) {
    for (i = 0; i < buffers->buf_count; i++) {
      if (buffers->left_buf[i] != NULL) {
	FLUID_FREE(buffers->left_buf[i]);
      }
    }
    FLUID_FREE(buffers->left_buf);
  }

  if (buffers->right_buf != NULL) {
    for (i = 0; i < buffers->buf_count; i++) {
      if (buffers->right_buf[i] != NULL) {
	FLUID_FREE(buffers->right_buf[i]);
      }
    }
    FLUID_FREE(buffers->right_buf);
  }

  if (buffers->fx_left_buf != NULL) {
    for (i = 0; i < buffers->fx_buf_count; i++) {
      if (buffers->fx_left_buf[i] != NULL) {
	FLUID_FREE(buffers->fx_left_buf[i]);
      }
    }
    FLUID_FREE(buffers->fx_left_buf);
  }

  if (buffers->fx_right_buf != NULL) {
    for (i = 0; i < buffers->fx_buf_count; i++) {
      if (buffers->fx_right_buf[i] != NULL) {
	FLUID_FREE(buffers->fx_right_buf[i]);
      }
    }
    FLUID_FREE(buffers->fx_right_buf);
  }  
}

void delete_fluid_rvoice_mixer(fluid_rvoice_mixer_t* mixer)
{
  fluid_return_if_fail(mixer != NULL);
  
  fluid_mixer_buffers_free(&mixer->buffers);
  if (mixer->fx.reverb)
    delete_fluid_revmodel(mixer->fx.reverb);
  if (mixer->fx.chorus)
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
            mixer->buffers.left_buf);

    fluid_ladspa_add_host_ports(ladspa_fx, "Main:R", audio_groups,
            mixer->buffers.right_buf);

    fluid_ladspa_add_host_ports(ladspa_fx, "Reverb:Send", 1,
            &mixer->buffers.fx_left_buf[SYNTH_REVERB_CHANNEL]);

    fluid_ladspa_add_host_ports(ladspa_fx, "Chorus:Send", 1,
            &mixer->buffers.fx_left_buf[SYNTH_CHORUS_CHANNEL]);
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
				  fluid_real_t*** left, fluid_real_t*** right)
{
  *left = mixer->buffers.left_buf;
  *right = mixer->buffers.right_buf;
  return mixer->buffers.buf_count;
}

int fluid_rvoice_mixer_get_fx_bufs(fluid_rvoice_mixer_t* mixer, 
                  fluid_real_t*** fx_left, fluid_real_t*** fx_right)
{
  *fx_left = mixer->buffers.fx_left_buf;
  *fx_right = mixer->buffers.fx_right_buf;
  return mixer->buffers.fx_buf_count;
}

int fluid_rvoice_mixer_get_bufcount(fluid_rvoice_mixer_t* mixer)
{
    return mixer->buffers.buf_blocks;
}

#if WITH_PROFILING
int fluid_rvoice_mixer_get_active_voices(fluid_rvoice_mixer_t* mixer)
{
	return mixer->active_voices;
}
#endif


/**
 * Synthesize audio into buffers
 * @param blockcount number of blocks to render, each having FLUID_BUFSIZE samples 
 * @return number of blocks rendered
 */
int 
fluid_rvoice_mixer_render(fluid_rvoice_mixer_t* mixer, int blockcount)
{
  fluid_profile_ref_var(prof_ref);
  
  mixer->current_blockcount = blockcount > mixer->buffers.buf_blocks ? 
      mixer->buffers.buf_blocks : blockcount;

  // Zero buffers
  fluid_mixer_buffers_zero(&mixer->buffers);
  fluid_profile(FLUID_PROF_ONE_BLOCK_CLEAR, prof_ref, mixer->active_voices,
                mixer->current_blockcount * FLUID_BUFSIZE);
  
    fluid_render_loop_singlethread(mixer);
  fluid_profile(FLUID_PROF_ONE_BLOCK_VOICES, prof_ref, mixer->active_voices,
                mixer->current_blockcount * FLUID_BUFSIZE);
    

  // Process reverb & chorus
  fluid_rvoice_mixer_process_fx(mixer);

  // Call the callback and pack active voice array
  fluid_rvoice_mixer_process_finished_voices(mixer);

  return mixer->current_blockcount;
}
