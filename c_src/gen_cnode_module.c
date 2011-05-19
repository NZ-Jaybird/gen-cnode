#include <string.h>

#include "gen_cnode_module.h"

//<<HERE>> Fix me
void gen_cnode_module_free( gen_cnode_module_t* module ){
    g_hash_table_destroy( module->funcs );
}

GHashTable* gen_cnode_module_init(){
    GHashTable* modules = NULL;
    //gen_cnode_module_t* bifs = NULL;
    

    if( !g_module_supported() ){
        fprintf(stderr, "g_module not supported!\n");
        goto gen_cnode_module_open_exit;
    }   

    modules = g_hash_table_new_full( g_str_hash, 
                                     g_str_equal,
                                     g_free,
                                     NULL );

    /*bifs = g_new0( gen_cnode_module_t, 1 );
    bifs->state = (struct gen_cnode_lib_state_s *)modules;
    bifs->funcs = g_hash_table_new_full( g_str_hash,
                                         g_str_equal,
                                         g_free,
                                         NULL );
    
    //Iterate through BIFs to build up gen_cnode module info
    for( i=0; gen_cnode_bifs[i]; i++ ){
         gen_cnode_bif_t* bif = gen_cnode_bifs[i];
         g_hash_table_insert( bifs->funcs, bif->name, (void*)bif->fp );
    }

    //Add gen_cnode entry
    g_hash_table_insert( modules, (void*)"gen_cnode", (void*)bifs );*/

    gen_cnode_module_open_exit:
    return modules;
}

int gen_cnode_module_load_init( gen_cnode_module_t* module ){
    int rc = 0;
    gboolean is_sym;
    gen_cnode_init_fp init_fp;
    gen_cnode_state_new_fp state_new_fp;

    //Create global library state
    is_sym = g_module_symbol( module->lib, "GEN_CNODE_STATE_NEW",
                              ((gpointer*)&state_new_fp) );
    if( is_sym ){
        module->state = state_new_fp(); 
    } else {
        module->state = NULL;
    }

    //Look for GEN_CNODE_INIT
    is_sym = g_module_symbol( module->lib, "GEN_CNODE_INIT", 
                              ((gpointer*)&init_fp) );
    if( is_sym && init_fp ){
        
        //Insert init_fp into our hashtable (Not really needed)
        g_hash_table_insert( module->funcs, (void*)"GEN_CNODE_INIT", (void*)init_fp );
    
        //Call the init fuction and record the result    
        rc = init_fp( module->state );
    }

    return rc;
}

void gen_cnode_module_load_exit( gen_cnode_module_t* module ){
    gboolean is_sym;
    gen_cnode_exit_fp exit_fp;

    //Lookup GEN_CNODE_EXIT and stash value if it exists
    is_sym =  g_module_symbol( module->lib, "GEN_CNODE_EXIT", 
                               ((gpointer*)&exit_fp));
    if( is_sym && exit_fp ){
        g_hash_table_insert( module->funcs, (void*)"GEN_CNODE_EXIT", (void*)exit_fp);
    }
}

int gen_cnode_module_load_required( gen_cnode_module_t* module ){
    int rc = 0;
    int i;
    gboolean sym = FALSE;
    const char* (*fp) (int) = NULL;

    if( !module || module->reqs ){
        rc = -EINVAL;
        goto load_required_exit;
    }

    sym = g_module_symbol( module->lib, 
                           "GEN_CNODE_REQUIRED", 
                           ((gpointer*)&fp) );

    if( !sym ){
        printf("DEBUG: No prereqs detected\n");     
    } else {

        for( i=0; fp(i); i++ ){
            GModule* lib = NULL;
            char* fullname = NULL;

            //Stitch together full filename based on LD_LIBRARY_PATH
            fullname = g_module_build_path( NULL, fp(i) );

            printf("DEBUG: Attempting to open %s...\n", fullname); 

            //Attempt to load the module
            lib = g_module_open( fullname, (GModuleFlags)0 );
            if( !lib ){
                rc = -EINVAL;
                g_free(fullname);
                goto load_required_exit; 
            }

            //Not intending on using this pointer as all symbols
            //for the library are bound globally, but will store
            //them for later removal should the module be unloaded.
            module->reqs = g_list_append(module->reqs, lib);

            g_free(fullname);
        }
    }

    load_required_exit:
    return rc;
}

// {gen_cnode, load, lib_name}
int gen_cnode_module_load( int argc,
                           char* args, 
                           GHashTable* modules,
                           ei_x_buff* resp ){

    int rc = 0;
    int i, index;
    char* lib_name = NULL;
    gchar* fullname = NULL;

    if( !args || !argc || !modules || !resp ){
        rc = -EINVAL;
        goto load_exit;
    }

    for( i=0, index=0; i < argc; i++ ){
        GModule *lib = NULL;
        gen_cnode_module_t* module = NULL;
        
        lib_name = g_new0( char, 256 );

        if( (rc = ei_decode_atom(args, &index, lib_name)) ){
            ei_x_format(resp, "{~a,~a}", "error", "not_a_string");
            goto load_exit;
        }

        //If the module already is loaded...skip to end
        if( (module = (gen_cnode_module_t*)g_hash_table_lookup(modules, lib_name)) ){
            ei_x_format(resp, "{~a,~a,~s}", 
                        "error", "already_loaded", lib_name);
            goto load_exit;;
        }

        //Stitch together full filename based on LD_LIBRARY_PATH
        fullname = g_module_build_path( NULL, lib_name );

        //Attempt to load the module
        lib = g_module_open( fullname, (GModuleFlags)0);//(G_MODULE_BIND_LAZY | G_MODULE_BIND_LOCAL) );
        if( !lib ){
            ei_x_format(resp, "{~a,~a,~s}", "error", "not_found", lib_name);
            goto load_exit;
        }

        //Create new module object for storage
        module = g_new0( gen_cnode_module_t, 1 );
        module->lib = lib;
        module->funcs =  g_hash_table_new_full( g_str_hash, 
                                                g_str_equal,
                                                g_free,
                                                NULL );
   
        //Globally load module prereqs
        if( (rc = gen_cnode_module_load_required(module)) ){
            ei_x_format(resp, "{~a,~a,~s}", 
                        "error", "failed_to_load_required", fullname);
            goto load_exit;
        }

        //Attempt to initialize the module
        rc = gen_cnode_module_load_init( module );
        if( rc < 0 ){
            ei_x_format(resp, "{~a,~a,~s}", 
                        "error", "failed_to_init", fullname);
            goto load_exit;
        }

        gen_cnode_module_load_exit( module );

        //Stash the module in the modules hash_table
        g_hash_table_insert( modules, lib_name, module );
     
        printf("DEBUG: loaded %s....\n", fullname);

        g_free(fullname); fullname = NULL;
    }

    ei_x_format(resp, "~a", "ok");

    load_exit:

    if( fullname ){
        g_free(fullname);
    }

    return rc;
}

gen_cnode_fp gen_cnode_module_lookup( gchar* lib,
                                      gchar* func,
                                      GHashTable* modules ){
    gen_cnode_module_t* module = NULL;
    gen_cnode_fp fp = NULL;

    if( !(module = (gen_cnode_module_t*)g_hash_table_lookup( modules, lib )) ){
        goto gen_cnode_module_lookup_exit;
    }

    if( (fp = (gen_cnode_fp)g_hash_table_lookup( module->funcs, func )) ){
        goto gen_cnode_module_lookup_exit;   
    }

    //Slow path: Find the symbol in the stashed GModule
    if( module->lib && g_module_symbol( module->lib, func, ((gpointer*)&fp) ) ){
        g_hash_table_insert( module->funcs, g_strdup(func), (void*)fp );
        
        printf( "Added new func:%s in lib: %s! hash_size = %d\n", 
                func, lib, g_hash_table_size( modules ) );
    } 
    
    gen_cnode_module_lookup_exit:
    return fp;
}

int gen_cnode_module_callback( gen_cnode_callback_t* callback,
                               GHashTable* modules,
                               ei_x_buff* resp ){
    int rc = 0;
    gen_cnode_module_t* module = NULL;
    gchar* lib = callback->lib;
    gchar* func = callback->func;
    gen_cnode_fp fp = NULL;

    if( !lib || !func || !modules ){
        goto gen_cnode_module_callback_exit;
    }

    if( !(module = (gen_cnode_module_t*)g_hash_table_lookup( modules, lib )) ){
        fprintf(stderr, "DEBUG: Library %s is not loaded!\n", lib);
        ei_x_format( resp, "{~a, ~a}", "error", "lib_not_loaded" );
        goto gen_cnode_module_callback_exit;
    } 
    
    fp = gen_cnode_module_lookup( lib, func, modules );
    if( !fp ){
        fprintf(stderr, "DEBUG: Function %s not defined in %s\n", func, lib);
        ei_x_format( resp, "{~a, ~a}", "error", "symbol_dne" );
        goto gen_cnode_module_callback_exit;
    }

    rc = fp( callback->argc, callback->argv, module->state, resp ); 

    gen_cnode_module_callback_exit:
    return rc;
}

