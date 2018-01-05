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



/*
 2002 : API design by Peter Hanappe and Antoine Schmitt
 August 2002 : Implementation by Antoine Schmitt as@gratin.org
               as part of the infiniteCD author project
               http://www.infiniteCD.org/
*/

#include "fluidsynth_priv.h"
#include "fluid_synth.h"
#include "fluid_midi.h"
#include "fluid_event_priv.h"

 /***************************************************************
 *
 *                           SEQUENCER BINDING
 */

struct _fluid_seqbind_t {
	fluid_synth_t* synth;
	fluid_sequencer_t* seq;
	fluid_sample_timer_t* sample_timer;
	fluid_seq_id_t client_id;
};
typedef struct _fluid_seqbind_t fluid_seqbind_t;


int fluid_seqbind_timer_callback(void* data, unsigned int msec);
void fluid_seq_fluidsynth_callback(unsigned int time, fluid_event_t* event, fluid_sequencer_t* seq, void* data);

/* Proper cleanup of the seqbind struct. */
void 
delete_fluid_seqbind(fluid_seqbind_t* seqbind) 
{
	fluid_return_if_fail(seqbind != NULL);

	if ((seqbind->client_id != -1) && (seqbind->seq != NULL)) {
		fluid_sequencer_unregister_client(seqbind->seq, seqbind->client_id);
		seqbind->client_id = -1;
	}

	if ((seqbind->sample_timer != NULL) && (seqbind->synth != NULL)) {
		delete_fluid_sample_timer(seqbind->synth, seqbind->sample_timer);
		seqbind->sample_timer = NULL;
	}

	FLUID_FREE(seqbind);
}

/** 
 * Registers a synthesizer as a destination client of the given sequencer.
 * The \a synth is registered with the name "fluidsynth".
 * @param seq Sequencer instance
 * @param synth Synthesizer instance
 * @returns Sequencer client ID, or #FLUID_FAILED on error.
 */
fluid_seq_id_t 
fluid_sequencer_register_fluidsynth (fluid_sequencer_t* seq, fluid_synth_t* synth)
{
	fluid_seqbind_t* seqbind;
	
	seqbind = FLUID_NEW(fluid_seqbind_t);
	if (seqbind == NULL) {
		fluid_log(FLUID_PANIC, "sequencer: Out of memory\n");
		return FLUID_FAILED;
	}

	seqbind->synth = synth;
	seqbind->seq = seq;
	seqbind->sample_timer = NULL;
	seqbind->client_id = -1;

	/* set up the sample timer */
	if (!fluid_sequencer_get_use_system_timer(seq)) {
		seqbind->sample_timer = 
			new_fluid_sample_timer(synth, fluid_seqbind_timer_callback, (void *) seqbind);
		if (seqbind->sample_timer == NULL) {
			fluid_log(FLUID_PANIC, "sequencer: Out of memory\n");
			delete_fluid_seqbind(seqbind);
			return FLUID_FAILED;
		}
	}

	/* register fluidsynth itself */
	seqbind->client_id = 
		fluid_sequencer_register_client(seq, "fluidsynth", fluid_seq_fluidsynth_callback, (void *)seqbind);
	if (seqbind->client_id == -1) {
		delete_fluid_seqbind(seqbind);
		return FLUID_FAILED;
	}

	return seqbind->client_id;
}

/* Callback for sample timer */
int
fluid_seqbind_timer_callback(void* data, unsigned int msec)
{
	fluid_seqbind_t* seqbind = (fluid_seqbind_t *) data;
	fluid_sequencer_process(seqbind->seq, msec);
	return 1;
}

/* Callback for midi events */
void 
fluid_seq_fluidsynth_callback(unsigned int time, fluid_event_t* evt, fluid_sequencer_t* seq, void* data)
{
	fluid_synth_t* synth;
	fluid_seqbind_t* seqbind = (fluid_seqbind_t *) data;
	synth = seqbind->synth;

    if(fluid_synth_handle_event(synth, evt) == FLUID_OK)
        return;
    
  switch (fluid_event_get_type(evt)) {

  case FLUID_SEQ_NOTE:
	  {
	  	unsigned int dur;
	  	fluid_synth_noteon(synth, fluid_event_get_channel(evt), fluid_event_get_key(evt), fluid_event_get_velocity(evt));
	  	dur = fluid_event_get_duration(evt);
	  	fluid_event_noteoff(evt, fluid_event_get_channel(evt), fluid_event_get_key(evt));
	  	fluid_sequencer_send_at(seq, evt, dur, 0);
	  }
  	break;

  case FLUID_SEQ_UNREGISTERING: /* free ourselves */
    seqbind->client_id = -1; /* avoid recursive call to fluid_sequencer_unregister_client */
    delete_fluid_seqbind(seqbind);
	break;

	default:
  	break;
	}
}

