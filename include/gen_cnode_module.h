#ifndef GEN_CNODE_MODULE_H
#define GEN_CNODE_MODULE_H

#include <stdio.h>
#include <glib.h>
#include <gmodule.h>

#include "gen_cnode.h"

typedef struct gen_cnode_module_s {
    GModule* lib;
    GList* reqs;
    GHashTable* funcs;
    struct gen_cnode_lib_state_s *state;
} gen_cnode_module_t;

typedef struct gen_cnode_module_entry_s {
    gen_cnode_fp fp;
} gen_cnode_module_entry_t;

typedef struct gen_cnode_callback_s {
    erlang_pid from;                    //Pid making callback
    gchar lib  [MAXATOMLEN + 1];        //Library name
    gchar func [MAXATOMLEN + 1];        //Function name    
    gchar* argv;                        //Pointer to first element of argument list
    guint32 argc;                       //Arity of argument list
    gchar* msg;                         //Orginal message
} gen_cnode_callback_t;

/* Creates a hash table of gen_cnode_module_t and populates gen_cnode BIFS */
GHashTable* gen_cnode_module_init();

/* Returns a gen_cnode_fp to the func_name specified. */
gen_cnode_fp gen_cnode_module_lookup( gchar* lib,
                                      gchar* func,
                                      GHashTable* modules );

/* Performs proper lookups and calls the specified function (if it exists) */
int gen_cnode_module_callback( gen_cnode_callback_t* callback,
                               GHashTable* modules, 
                               ei_x_buff* resp );

/* BIF which attempts to load the specified gen_cnode library */
int gen_cnode_module_load( int argc, char* args, GHashTable* modules, ei_x_buff* resp );


#endif
