
#include "fluid_instpatch.h"
#include "fluid_hash.h"
#include "fluid_sfont.h"

#include <libinstpatch/libinstpatch.h>

typedef struct _fluid_instpatch_font_t
{
    char name[256];
    IpatchDLS2 *dls;
    fluid_hashtable_t *voice_cache_hash;
} fluid_instpatch_font_t;


typedef struct _fluid_instpatch_preset_t
{
    fluid_instpatch_font_t *parent_sfont;
    IpatchDLS2Inst *item;

    /* pointer to name of the preset, duplicated from item, alloced by glib */
    char *name;
    int bank;
    int prog;
} sfloader_preset_data_t;

// private struct for storing additional data for each instpatch voice
typedef struct _instpatch_voice_user_data
{
    /* pointer to the sample store that holds the PCM */
    IpatchSampleStoreCache* sample_store;
    
    /* pointer to a preallocated fluid_sample_t that we can use during noteon for this voice */
    fluid_sample_t* sample;
} fluid_instpatch_voice_user_data_t;


/* max voices per instrument (voices exceeding this will not sound) */
enum
{
    MAX_INST_VOICES = 128,
};

/* defer libinstpatch init to fluid_instpatch.c to avoid #include "libinstpatch.h" */
void fluid_instpatch_init(void)
{
    /* initialize libInstPatch */
    ipatch_init();
}

static void delete_fluid_instpatch(fluid_instpatch_font_t *pfont);

static const char *fluid_instpatch_sfont_get_name(fluid_sfont_t *sfont);
static fluid_preset_t *fluid_instpatch_sfont_get_preset(fluid_sfont_t *sfont, int bank, int prenum);

/* sfloader callback to get the name of a preset */
static const char *
fluid_instpatch_preset_get_name(fluid_preset_t *preset)
{
    sfloader_preset_data_t *preset_data = fluid_preset_get_data(preset);
    return preset_data->name;
}

/* sfloader callback to get the bank number of a preset */
static int
fluid_instpatch_preset_get_banknum(fluid_preset_t *preset)
{
    sfloader_preset_data_t *preset_data = fluid_preset_get_data(preset);
    return preset_data->bank;
}

/* sfloader callback to get the preset number of a preset */
static int
fluid_instpatch_preset_get_num(fluid_preset_t *preset)
{
    sfloader_preset_data_t *preset_data = fluid_preset_get_data(preset);
    return preset_data->prog;
}


/* sfloader callback for a noteon event */
static int
fluid_instpatch_preset_noteon(fluid_preset_t *preset, fluid_synth_t *synth, int chan, int key, int vel)
{
    guint16 index_array[MAX_INST_VOICES];		/* voice index array */
    int sel_values[IPATCH_SF2_VOICE_CACHE_MAX_SEL_VALUES];
    fluid_mod_t *fmod = g_alloca(fluid_mod_sizeof());
    fluid_voice_t *flvoice;

    sfloader_preset_data_t *preset_data = fluid_preset_get_data(preset);

    int i, voice_count, voice_num, ret = FLUID_FAILED;
    GSList *p;
    
    /* lookup the voice cache that we've created on loading */
    IpatchSF2VoiceCache *cache = fluid_hashtable_lookup(preset_data->parent_sfont->voice_cache_hash, preset_data->item);
    g_object_ref(cache);

    for(i = 0; i < cache->sel_count; i++)
    {
        IpatchSF2VoiceSelInfo *sel_info = &cache->sel_info[i];

        switch(sel_info->type)
        {
        case IPATCH_SF2_VOICE_SEL_NOTE:
            sel_values[i] = key;
            break;

        case IPATCH_SF2_VOICE_SEL_VELOCITY:
            sel_values[i] = vel;
            break;

        default:
            /* match any; NOTE: according to documentation this should be IPATCH_SF2_VOICE_SEL_WILDCARD */
            sel_values[i] = -1;
            break;
        }
    }

    voice_count = ipatch_sf2_voice_cache_select(cache, sel_values, index_array, MAX_INST_VOICES);

    /* loop over matching voice indexes */
    for(voice_num = 0; voice_num < voice_count; voice_num++)
    {
        IpatchSF2GenArray *gen_array;
        fluid_sample_t *fsample;
        
        IpatchSF2Voice *voice = IPATCH_SF2_VOICE_CACHE_GET_VOICE(cache, index_array[voice_num]);

        if(!voice->sample_store)
        {
            /* For ROM and other non-readable samples */
            continue;
        }
        
        fsample = ((fluid_instpatch_voice_user_data_t*)voice->user_data)->sample;

        ret = fluid_sample_set_sound_data(fsample,
                                        ipatch_sample_store_cache_get_location(IPATCH_SAMPLE_STORE_CACHE(voice->sample_store)),
                                        NULL,
                                        voice->sample_size,
                                        voice->rate,
                                        TRUE
                                    );
        if(FLUID_UNLIKELY(ret == FLUID_FAILED))
        {
            FLUID_LOG(FLUID_ERR, "fluid_sample_set_sound_data() failed");
            goto error_rec;
        }
        
        ret = fluid_sample_set_loop(fsample, voice->loop_start, voice->loop_end);
        if(FLUID_UNLIKELY(ret == FLUID_FAILED))
        {
            FLUID_LOG(FLUID_ERR, "fluid_sample_set_loop() failed");
            goto error_rec;
        }
        
        ret = fluid_sample_set_pitch(fsample, voice->root_note, voice->fine_tune);
        if(FLUID_UNLIKELY(ret == FLUID_FAILED))
        {
            FLUID_LOG(FLUID_ERR, "fluid_sample_set_pitch() failed");
            goto error_rec;
        }

        /* allocate the FluidSynth voice */
        flvoice = fluid_synth_alloc_voice(synth, fsample, chan, key, vel);

        if(flvoice == NULL)
        {
            g_object_unref(cache);
            ret = FLUID_FAILED;
            goto error_rec;
        }

        /* set only those generator parameters that are set */
        gen_array = &voice->gen_array;

        for(i = 0; i < IPATCH_SF2_GEN_COUNT; i++)
        {
            if(IPATCH_SF2_GEN_ARRAY_TEST_FLAG(gen_array, i))
            {
                fluid_voice_gen_set(flvoice, i, (float)(gen_array->values[i].sword));
            }
        }

        p = voice->mod_list;

        while(p)
        {
            IpatchSF2Mod *mod = p->data;

            fluid_mod_set_dest(fmod, mod->dest);
            fluid_mod_set_source1(fmod,
                                  mod->src & IPATCH_SF2_MOD_MASK_CONTROL,
                                  ((mod->src & (IPATCH_SF2_MOD_MASK_DIRECTION | IPATCH_SF2_MOD_MASK_POLARITY | IPATCH_SF2_MOD_MASK_TYPE)) >> IPATCH_SF2_MOD_SHIFT_DIRECTION)
                                  | ((mod->src & IPATCH_SF2_MOD_MASK_CC) ? FLUID_MOD_CC : 0));

            fluid_mod_set_source2(fmod,
                                  mod->amtsrc & IPATCH_SF2_MOD_MASK_CONTROL,
                                  ((mod->amtsrc & (IPATCH_SF2_MOD_MASK_DIRECTION | IPATCH_SF2_MOD_MASK_POLARITY | IPATCH_SF2_MOD_MASK_TYPE)) >> IPATCH_SF2_MOD_SHIFT_DIRECTION)
                                  | ((mod->amtsrc & IPATCH_SF2_MOD_MASK_CC) ? FLUID_MOD_CC : 0));

            fluid_mod_set_amount(fmod, mod->amount);
            fluid_voice_add_mod(flvoice, fmod, FLUID_VOICE_OVERWRITE);

            p = p->next;
        }

        fluid_synth_start_voice(synth, flvoice);

        /* sample store reference taken over by fsample structure */
    }
    ret = FLUID_OK;

error_rec:
    
    g_object_unref(cache);
    return ret;
}




/* sfloader callback to get a patch file name */
static const char *
fluid_instpatch_sfont_get_name(fluid_sfont_t *sfont)
{
    fluid_instpatch_font_t *sfont_data = fluid_sfont_get_data(sfont);
    return sfont_data->name;
}

static void delete_fluid_instpatch_preset(sfloader_preset_data_t *preset_data)
{
    g_free(preset_data->name);

    /* -- remove item reference */
    g_object_unref(preset_data->item);

    FLUID_FREE(preset_data);
}

static void fluid_instpatch_preset_free(fluid_preset_t *preset)
{
    delete_fluid_instpatch_preset(fluid_preset_get_data(preset));
    delete_fluid_preset(preset);
}

/* sfloader callback to get a preset (instrument) by bank and preset number */
static fluid_preset_t *
fluid_instpatch_sfont_get_preset(fluid_sfont_t *sfont, int bank, int prenum)
{
    fluid_instpatch_font_t *sfont_data = fluid_sfont_get_data(sfont);

    sfloader_preset_data_t *preset_data;
    fluid_preset_t *preset;

    /* no need to ref item, already referenced by find */
    IpatchDLS2Inst *item = ipatch_dls2_find_inst(sfont_data->dls, NULL, bank, prenum, NULL);

    if(item == NULL)
    {
        return (NULL);
    }

    preset_data = FLUID_NEW(sfloader_preset_data_t);

    if(preset_data == NULL)
    {
        g_object_unref(item);
        return NULL;
    }

    FLUID_MEMSET(preset_data, 0, sizeof(*preset_data));

    preset_data->parent_sfont = sfont_data;
    preset_data->item = item;
    /* save name, bank and preset for quick lookup */
    preset_data->bank = bank;
    preset_data->prog = prenum;
    g_object_get(preset_data->item, "name", &preset_data->name, NULL);

    preset = new_fluid_preset(sfont,
                              fluid_instpatch_preset_get_name,
                              fluid_instpatch_preset_get_banknum,
                              fluid_instpatch_preset_get_num,
                              fluid_instpatch_preset_noteon,
                              fluid_instpatch_preset_free);

    if(preset == NULL)
    {
        delete_fluid_instpatch_preset(preset_data);
        return NULL;
    }

    fluid_preset_set_data(preset, preset_data);

    return preset;
}

static fluid_instpatch_voice_user_data_t* new_fluid_instpatch_voice_user_data(IpatchSampleStoreCache* sample_store)
{
    fluid_instpatch_voice_user_data_t* dat = FLUID_NEW(fluid_instpatch_voice_user_data_t);
    fluid_sample_t* sample = new_fluid_sample();
    
    if(dat == NULL || sample == NULL)
    {
        FLUID_FREE(dat);
        delete_fluid_sample(sample);
        return NULL;
    }
    
    dat->sample = sample;
    
    /* Keep sample store cached by doing a dummy open */
    ipatch_sample_store_cache_open(sample_store);
    dat->sample_store = sample_store;
    return dat;
}

static void fluid_instpatch_on_voice_user_data_destroy(gpointer user_data)
{
    fluid_instpatch_voice_user_data_t* dat = user_data;
    
    delete_fluid_sample(dat->sample);
    ipatch_sample_store_cache_close(dat->sample_store);
    FLUID_FREE(dat);
}

static int cache_instrument(fluid_instpatch_font_t *patchfont, IpatchDLS2Inst *item)
{
    IpatchConverter *conv;
    IpatchSF2VoiceCache *cache;
    int i, count;

    /* create SF2 voice cache converter */
    conv = ipatch_create_converter(G_OBJECT_TYPE(item), IPATCH_TYPE_SF2_VOICE_CACHE);

    /* no SF2 voice cache converter for this item type? */
    if(conv == NULL)
    {
        FLUID_LOG(FLUID_ERR, "Unable to find a voice cache converter for this type");
        return FLUID_FAILED;
    }

    cache = ipatch_sf2_voice_cache_new(NULL, 0);

    if(cache == NULL)
    {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        g_object_unref(conv);
        return FLUID_FAILED;
    }

    ipatch_converter_add_input(conv, G_OBJECT(item));
    ipatch_converter_add_output(conv, G_OBJECT(cache));

    if(!ipatch_converter_convert(conv, NULL))
    {
        FLUID_LOG(FLUID_ERR, "Failed to convert DLS inst to SF2 voices");
        g_object_unref(cache);
        g_object_unref(conv);
        return FLUID_FAILED;
    }

    g_object_unref(conv);
    conv = NULL;

    /* Use voice->user_data to close open cached stores */
    cache->voice_user_data_destroy = fluid_instpatch_on_voice_user_data_destroy;

    /* loop over voices and load sample data into RAM */
    count = cache->voices->len;

    for(i = 0; i < count; i++)
    {
        IpatchSF2Voice *voice = &g_array_index(cache->voices, IpatchSF2Voice, i);

        if(!ipatch_sf2_voice_cache_sample_data(voice, NULL))
        {
            FLUID_LOG(FLUID_ERR, "ipatch_sf2_voice_cache_sample_data() failed");
            g_object_unref(cache);
            return FLUID_FAILED;
        }
        
        voice->user_data = new_fluid_instpatch_voice_user_data(IPATCH_SAMPLE_STORE_CACHE(voice->sample_store));
    }

    /* !! hash takes over cache reference */
    fluid_hashtable_insert(patchfont->voice_cache_hash, item, cache);

    return FLUID_OK;
}


fluid_instpatch_font_t *new_fluid_instpatch(fluid_sfont_t *sfont, const fluid_file_callbacks_t *fcbs, const char *filename)
{
    fluid_instpatch_font_t *patchfont = NULL;
    GError *err = NULL;
    IpatchDLSReader *reader = NULL;
    IpatchDLSFile *file = NULL;
    IpatchFileHandle *handle = NULL;

    fluid_return_val_if_fail(filename != NULL, NULL);

    if((patchfont = FLUID_NEW(fluid_instpatch_font_t)) == NULL)
    {
        return NULL;
    }

    FLUID_MEMSET(patchfont, 0, sizeof(*patchfont));

    FLUID_STRNCPY(&patchfont->name[0], filename, sizeof(patchfont->name));

    /* open a file, we get a reference */
    if((file = ipatch_dls_file_new()) == NULL)
    {
        FLUID_FREE(patchfont);

        return NULL;
    }

    /* TODO
    ipatch_file_set_iofuncs(file, fcbs);
    */

    /* ipatch_file_open() references the file again */
    if((handle = ipatch_file_open(IPATCH_FILE(file), filename, "r", &err)) == NULL)
    {
        FLUID_LOG(FLUID_ERR, "ipatch_file_open() failed with error: '%s'", ipatch_gerror_message(err));

        g_object_unref(file);
        FLUID_FREE(patchfont);

        return NULL;
    }

    /* get rid of the reference we own, we dont need it any longer */
    g_object_unref(file);
    file = NULL;

    /* open a reader, this gives us a reference */
    if((reader = ipatch_dls_reader_new(handle)) == NULL)
    {
        ipatch_file_close(handle);
        FLUID_FREE(patchfont);

        return NULL;
    }

    patchfont->dls = ipatch_dls_reader_load(reader, &err);

    /* unref the reader directly afterwards, not needed any longer */
    g_object_unref(reader);
    reader = NULL;

    if(patchfont->dls == NULL)
    {
        FLUID_LOG(FLUID_ERR, "ipatch_dls_reader_new() failed with error: '%s'", ipatch_gerror_message(err));

        // reader has already been unrefed, i.e. no need to call ipatch_file_close()

        FLUID_FREE(patchfont);

        return NULL;
    }

    /* at this point everything is owned by the IpatchDLS2*, no need for custom cleanups any longer */

    do
    {
        IpatchIter iter;
        IpatchDLS2Inst *inst;
        gboolean success;

        if((patchfont->voice_cache_hash = new_fluid_hashtable_full(NULL, NULL, NULL, g_object_unref)) == NULL)
        {
            break;
        }

        success = ipatch_container_init_iter(IPATCH_CONTAINER(patchfont->dls), &iter, IPATCH_TYPE_DLS2_INST);

        if(success == FALSE)
        {
            break;
        }

        inst = ipatch_dls2_inst_first(&iter);
        
        if(inst == NULL)
        {
            FLUID_LOG(FLUID_ERR, "A soundfont file was accepted by libinstpatch, but it doesn't contain a single instrument. Dropping the whole file.");
            break;
        }

        /* loop over instruments */
        do
        {
            if(cache_instrument(patchfont, inst) == FLUID_FAILED)
            {
                int bank, prog;
                ipatch_dls2_inst_get_midi_locale(inst, &bank, &prog);
                FLUID_LOG(FLUID_WARN, "Failed to cache DLS instrument bank %d , prog %d", bank, prog);
            }

//                 IpatchIter region_iter;
//                 success = ipatch_container_init_iter ((IpatchContainer *)patchfont->dls, &region_iter, IPATCH_TYPE_DLS2_INST);
//                 fluid_return_val_if_fail (success != FALSE, NULL);
//
//                 IpatchDLS2Region *region = ipatch_dls2_region_first (&region_iter);
//                 while (region)		/* loop over regions */
//                 {
//                     if (ipatch_dls2_region_peek_sample (region) == sample)
//                     {
//                         g_object_ref (region); /* ++ ref region for new iterator */
//                     }
//                     region = ipatch_dls2_region_next (&region_iter);
//                 }

            inst = ipatch_dls2_inst_next(&iter);
        }
        while(inst);

        return patchfont;
    }
    while(0);

    delete_fluid_instpatch(patchfont);
    return NULL;
}

static void delete_fluid_instpatch(fluid_instpatch_font_t *pfont)
{
    fluid_return_if_fail(pfont != NULL);

    delete_fluid_hashtable(pfont->voice_cache_hash);

    // also unrefs IpatchDLSFile and IpatchFileHandle
    g_object_unref(pfont->dls);

    FLUID_FREE(pfont);
}

static int fluid_instpatch_sfont_delete(fluid_sfont_t *sfont)
{
    fluid_return_val_if_fail(sfont != NULL, -1);

    delete_fluid_instpatch(fluid_sfont_get_data(sfont));
    delete_fluid_sfont(sfont);

    return 0;
}

static fluid_sfont_t *fluid_instpatch_loader_load(fluid_sfloader_t *loader, const char *filename)
{
    fluid_instpatch_font_t *patchfont = NULL;
    fluid_sfont_t *sfont = NULL;

    sfont = new_fluid_sfont(fluid_instpatch_sfont_get_name,
                            fluid_instpatch_sfont_get_preset,
                            NULL,
                            NULL,
                            fluid_instpatch_sfont_delete);

    if(sfont == NULL)
    {
        return NULL;
    }

    if((patchfont = new_fluid_instpatch(sfont, &loader->file_callbacks, filename)) == NULL)
    {
        delete_fluid_sfont(sfont);
        return NULL;
    }

    fluid_sfont_set_data(sfont, patchfont);
    return sfont;
}

fluid_sfloader_t *new_fluid_instpatch_loader(fluid_settings_t *settings)
{
    fluid_sfloader_t *loader;
    fluid_return_val_if_fail(settings != NULL, NULL);

    loader = new_fluid_sfloader(fluid_instpatch_loader_load, delete_fluid_sfloader);

    if(loader == NULL)
    {
        FLUID_LOG(FLUID_ERR, "Out of memory");
        return NULL;
    }

    fluid_sfloader_set_data(loader, settings);

    return loader;
}
