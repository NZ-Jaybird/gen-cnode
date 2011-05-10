#include "gen_cnode_module.h"

typedef struct gen_cnode_bif_s {
    gchar* name;
    gen_cnode_fp fp;
} gen_cnode_bif_t;

static gen_cnode_bif_t* gen_cnode_bifs[] = {
    &(gen_cnode_bif_t){ "load", (gen_cnode_fp)gen_cnode_module_load },
    NULL 
};

//<<HERE>> Fix me
void gen_cnode_module_free( gen_cnode_module_t* module ){
    g_hash_table_destroy( module->funcs );
}

GHashTable* gen_cnode_module_init(){
    int i;
    GHashTable* modules = NULL;
    gen_cnode_module_t* bifs = NULL;
    

    if( !g_module_supported() ){
        fprintf(stderr, "g_module not supported!\n");
        goto gen_cnode_module_open_exit;
    }   

    modules = g_hash_table_new_full( g_str_hash, 
                                     g_str_equal,
                                     g_free,
                                     NULL );

    bifs = g_new0( gen_cnode_module_t, 1 );
    bifs->state = (void*)modules;
    bifs->funcs = g_hash_table_new_full( g_str_hash,
                                         g_str_equal,
                                         g_free,
                                         NULL );
    
    //Iterate through BIFs to build up gen_cnode module info
    for( i=0; gen_cnode_bifs[i]; i++ ){
         gen_cnode_bif_t* bif = gen_cnode_bifs[i];
         g_hash_table_insert( bifs->funcs, bif->name, bif->fp );
    }

    //Add gen_cnode entry
    g_hash_table_insert( modules, "gen_cnode", bifs );

    gen_cnode_module_open_exit:
    return modules;
}

int gen_cnode_module_load_init( gen_cnode_module_t* module, ETERM** result ){
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
        g_hash_table_insert( module->funcs, "GEN_CNODE_INIT", init_fp );
    
        //Call the init fuction and record the result    
        rc = init_fp( module->state );
        if( rc < 0 ){
            *result = erl_format( "{error, failed_to_initialize}" );
            fprintf( stderr, "Library failed to initialize! rc = %d\n", rc );
        }
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
        g_hash_table_insert( module->funcs, "GEN_CNODE_EXIT", exit_fp);
    }
}

void gen_cnode_module_load_internal( ETERM* args,
                                     ETERM** results,
                                     guint32 i,
                                     GHashTable* modules){
    int rc = 0;
    ETERM* lib_atom = NULL; 
    gchar *lib_name = NULL, *fullname = NULL;
    gen_cnode_module_t* module;
    GModule* lib = NULL;

    if( ERL_IS_EMPTY_LIST(args) ){
        return;
    } 

    lib_atom = erl_hd( args );

    if( !ERL_IS_ATOM(lib_atom) ){
        results[i] = erl_format("{ error, bad_arg }");
    }

    lib_name = ERL_ATOM_PTR( lib_atom );

    erl_free_term( lib_atom );

    //If the module already is loaded...skip to end
    if( (module = g_hash_table_lookup(modules, lib_name)) ){
        results[i] = erl_format( "{ok, already_loaded}" );
        goto gen_cnode_module_load_recurse;
    }

    //Stitch together full filename based on LD_LIBRARY_PATH
    fullname = g_module_build_path( NULL, lib_name );

    //Attempt to load the module
    lib = g_module_open( fullname, G_MODULE_BIND_LAZY | G_MODULE_BIND_LOCAL );
    if( !lib ){
        results[i] = erl_format( "{error, failed_to_load}" );
        fprintf( stderr, "Failed to load %s!\n", fullname );
        goto gen_cnode_module_load_recurse;
    }

    //Create new module object for storage
    module = g_new0( gen_cnode_module_t, 1 );
    module->lib = lib;
    module->funcs =  g_hash_table_new_full( g_str_hash, 
                                            g_str_equal,
                                            g_free,
                                            NULL );
    
    //Attempt to initialize the module
    rc = gen_cnode_module_load_init( module, results + i );
    if( rc < 0 ){
        gen_cnode_module_free( module );
        goto gen_cnode_module_load_recurse;
    }

    gen_cnode_module_load_exit( module );

    //Stash the module in the modules hash_table
    g_hash_table_insert( modules, lib_name, module );
    
    fprintf( stdout, "Loaded %s...\n", fullname );

    results[i] = erl_format( "{ok, ~a}", lib_name ); 

    gen_cnode_module_load_recurse:
    gen_cnode_module_load_internal( erl_tl(args), results, (i + 1), modules );

    g_free( fullname );
}

// {gen_cnode, load, lib_name}
int gen_cnode_module_load( ETERM* args, 
                           ETERM** resp, 
                           GHashTable* modules ){
    int length = erl_length( args );
    ETERM** results = NULL;
 
    if( length <= 0 ){
        goto gen_cnode_module_load_exit;
    } 

    results = g_new0( ETERM*, length );

    gen_cnode_module_load_internal( args, results, 0, modules );

    *resp = erl_mk_list( results, length );

    //<<HERE>> Do I need to free resp array?

    gen_cnode_module_load_exit:
    return 0;
}

gen_cnode_fp gen_cnode_module_lookup( gchar* lib,
                                      gchar* func,
                                      GHashTable* modules ){
    gen_cnode_module_t* module = NULL;
    gen_cnode_fp fp = NULL;

    if( !(module = g_hash_table_lookup( modules, lib )) ){
        goto gen_cnode_module_lookup_exit;
    }

    if( (fp = g_hash_table_lookup( module->funcs, func )) ){
        goto gen_cnode_module_lookup_exit;   
    }

    //Slow path: Find the symbol in the stashed GModule
    if( module->lib && g_module_symbol( module->lib, func, ((gpointer*)&fp) ) ){
        g_hash_table_insert( module->funcs, g_strdup(func), fp );
        
        printf( "Added new func:%s in lib: %s! hash_size = %d\n", 
                func, lib, g_hash_table_size( modules ) );
    } 
    
    gen_cnode_module_lookup_exit:
    return fp;
}

int gen_cnode_module_callback( gen_cnode_callback_t* callback,
                               GHashTable* modules,
                               ei_x_buff** resp ){
    int rc = 0;
    /*gen_cnode_module_t* module = NULL;
    gchar* lib = callback->lib;
    gchar* func = callback->func;
    gen_cnode_fp fp = NULL;

    if( !lib || !func || !modules ){
        goto gen_cnode_module_callback_exit;
    }

    if( !(module = g_hash_table_lookup( modules, lib )) ){
        *resp = erl_format( "{error, lib_not_loaded}" );
        goto gen_cnode_module_callback_exit;
    } 
    
    fp = gen_cnode_module_lookup( lib, func, modules );
    if( !fp ){
        *resp = erl_format( "{errror, {symbol_dne, ~a}}", func );
        goto gen_cnode_module_callback_exit;
    }

    rc = fp( callback->args, resp, module->state ); 

    gen_cnode_module_callback_exit:*/
    return rc;
}

