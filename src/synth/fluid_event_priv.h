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


#ifndef _FLUID_EVENT_PRIV_H
#define _FLUID_EVENT_PRIV_H

#include "fluidsynth.h"
#include "fluid_sys.h"

/* Private data for event */
struct _fluid_event_t {
	unsigned int time;
	int type;
	fluid_seq_id_t src;
	fluid_seq_id_t dest;
	int channel;
    union
    {
        short key;
        short control;
        short pitch;
    } param1;
    
    union
    {
        short vel;
        short value;
        short id; //?? unused ?
    } param2;
    
	unsigned int duration;
	void* data;
};

unsigned int fluid_event_get_time(fluid_event_t* evt);
void fluid_event_set_time(fluid_event_t* evt, unsigned int time);

void fluid_event_tempo(fluid_event_t* evt, int tempo);

void fluid_event_clear(fluid_event_t* evt);
void fluid_event_to_str(fluid_event_t* evt, char*buf, int len);

/* private data for sorter + heap */
enum fluid_evt_entry_type {
  FLUID_EVT_ENTRY_INSERT = 0,
  FLUID_EVT_ENTRY_REMOVE
};

typedef struct _fluid_evt_entry fluid_evt_entry;
struct _fluid_evt_entry {
	fluid_evt_entry *next;
	short entryType;
	fluid_event_t evt;
};

#define HEAP_WITH_DYNALLOC 1
/* #undef HEAP_WITH_DYNALLOC */

typedef struct _fluid_evt_heap_t {
#ifdef HEAP_WITH_DYNALLOC
  fluid_evt_entry* freelist;
  fluid_mutex_t mutex;
#else
	fluid_evt_entry* head;
	fluid_evt_entry* tail;
	fluid_evt_entry pool;
#endif
} fluid_evt_heap_t;

fluid_evt_heap_t* _fluid_evt_heap_init(int nbEvents);
void _fluid_evt_heap_free(fluid_evt_heap_t* heap);
fluid_evt_entry* _fluid_seq_heap_get_free(fluid_evt_heap_t* heap);
void _fluid_seq_heap_set_free(fluid_evt_heap_t* heap, fluid_evt_entry* evt);

#endif /* _FLUID_EVENT_PRIV_H */
