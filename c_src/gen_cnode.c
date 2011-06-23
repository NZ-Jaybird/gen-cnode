//#include <iostream>

#include <stdio.h>
#include <stdbool.h>
#include <glib.h>

#include "gen_cnode.h"
#include "gen_cnode_options.h"      //Command line options
#include "gen_cnode_net.h"          //Net utilities
#include "gen_cnode_module.h"       //Module support

#include "erl_interface.h"
#include "ei.h"

#define GEN_CNODE_RECV_MAX 4096 //4KB of data max

typedef struct gen_cnode_state_s {
    int erl_fd;                 //FD for outgoing messages
    gboolean running;           //Have we received a stop commmand?
    gboolean receiving;
    gen_cnode_t node;           //Node network specifics: fd, addr_in, ei_cnode
    gen_cnode_opts_t* opts;     //gen_cnode options
    GThread* msgThread;         //Erlang message handler
    GHashTable* modules;        //Library hashtable
    GAsyncQueue* parentQueue;   //Parent callback queue
    GThreadPool* worker_pool;   //Thread pool reserved for handling callback
} gen_cnode_state_t;

typedef struct gen_cnode_bif_s {
    gchar* name;
    gen_cnode_fp fp;
} gen_cnode_bif_t;

//gen_cnode BIFs
int gen_cnode_load( int, char*, gen_cnode_state_t*, ei_x_buff* );
int gen_cnode_stop( int, char*, gen_cnode_state_t*, ei_x_buff* );
int gen_cnode_link( int, char*, gen_cnode_state_t*, ei_x_buff* );

static gen_cnode_bif_t* gen_cnode_bifs[] = {
        &(gen_cnode_bif_t){ "load", (gen_cnode_fp)gen_cnode_load },
        &(gen_cnode_bif_t){ "stop", (gen_cnode_fp)gen_cnode_stop },
        NULL 
};

//Helpers
int gen_cnode_check_args( gen_cnode_opts_t* opts );
int gen_cnode_init( gen_cnode_opts_t* opts, gen_cnode_state_t* state );
int gen_cnode_handle_connection( gen_cnode_state_t* state );
void gen_cnode_handle_callback( gen_cnode_callback_t* callback, 
                                gen_cnode_state_t* state );
void gen_cnode_free_callback( gen_cnode_callback_t* callback );
void gen_cnode_exit(gen_cnode_state_t* state);

int main( int argc, char** argv ){
    int rc = 0;
    gen_cnode_state_t* state = NULL;
    GOptionGroup *main_group = NULL;
    GOptionContext *main_context = NULL;
    GError *error = NULL;

    //Define the option group
    main_group = g_option_group_new( gen_cnode_opt_sname, 
                                     gen_cnode_opt_lname, 
                                     gen_cnode_opt_shows, 
                                     &gen_cnode_opts, NULL );

    //Add the default entries
    g_option_group_add_entries( main_group, gen_cnode_opt_entries ); 
    
    //Define the main context
    main_context = g_option_context_new( gen_cnode_opt_lname );
   
    //Set to default options
    g_option_context_set_main_group( main_context, main_group );

    //Parse our command line args
    g_option_context_parse( main_context, &argc, &argv, &error );
    if( error ){
        rc = -1;
        fprintf( stderr, "%s\n", error->message );
        goto main_exit;
    }

    //Sanity check -- required args: -P
    rc = gen_cnode_check_args( &gen_cnode_opts );
    if( rc ){
        goto main_exit;
    }

    state = g_new0( gen_cnode_state_t, 1 );

    /* Setup global state, dlopen library, 
     * bind to localhost:<port>, and setup with epmd */
    rc = gen_cnode_init( &gen_cnode_opts, state );
    if( rc ){
        fprintf( stderr, "gen_cnode_init failed! rc = %d\n", rc );
        goto main_exit;
    }

    /* Handle all callbacks with parent as the actor */
    while( state->running ){
        gen_cnode_callback_t* cb = NULL;
        GTimeVal tmo;

        //Calculate TMO
        g_get_current_time(&tmo), g_time_val_add(&tmo, 1000);
       
        //When a callback arrives, perform the action 
        cb = (gen_cnode_callback_t*)g_async_queue_timed_pop(state->parentQueue, &tmo);
        if( cb ){
            gen_cnode_handle_callback(cb, state);    
        } 
    }

    main_exit:

    if( main_context ){
        g_option_context_free(main_context);
    }

    gen_cnode_exit( state );
    return rc;
}

int gen_cnode_check_args( gen_cnode_opts_t* opts ){
    int rc = 0;

    if( !(opts->name) ){
        rc = -EINVAL;
        fprintf( stderr, "You must specify a node name (-n)! "
                         "Try --help\n");
        goto check_args_exit;
    }

    if( opts->port == 0 ){
        rc = -EINVAL;
        fprintf( stderr, "You must specify a non-zero port (-p)! "
                 "Try --help-all\n");
    }

    if( (opts->workers < 0) && (opts->workers != -1) ){
        rc = -EINVAL;
        fprintf( stderr, "Number of worker threads must be non-zero!\n");
    }

    check_args_exit:
    return rc;
}


int gen_cnode_init( gen_cnode_opts_t* opts, gen_cnode_state_t* state ){
    int rc = 0;
    int i;
    gen_cnode_module_t* bifs = NULL;

    //Setup localhost communication to Erlang side
    rc = gen_cnode_net_init( opts->name,
                             opts->cookie, 
                             opts->port, 
                             opts->creation, 
                             &(state->node) );
    if( rc ){
        fprintf( stderr, "gen_cnode failed to initialize!\n"
                         "Ensure id/name is unique and specified"
                         " port is unused!\n" );
        goto gen_cnode_init_exit;
    }

    state->modules = gen_cnode_module_init();

    //Register Built-In Functions
    bifs = g_new0( gen_cnode_module_t, 1 );
    bifs->state = (struct gen_cnode_lib_state_s *)state;
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
    g_hash_table_insert( state->modules, (void*)"gen_cnode", (void*)bifs );

    if( !g_thread_supported() ){
        g_thread_init(NULL);
    } else {
        rc = -1;
        fprintf(stderr, "Glib threading not supported!\n");
        goto gen_cnode_init_exit;
    }

    //Initiate worker (non-inline) callback pool
    if( opts->workers ){

        //Setup connection pool
        state->worker_pool = g_thread_pool_new( (GFunc)gen_cnode_handle_callback,
                                                state, opts->workers, FALSE, FALSE );
        if( !(state->worker_pool) ){
            rc = -1;
            fprintf( stderr, "g_thread_pool_new failed!\n" );
            goto gen_cnode_init_exit;
        }
    } else {
        printf("DEBUG: Worker thread pool disabled!\n");
    }

    //Setup parent thread (inline) async queue
    state->parentQueue = g_async_queue_new_full((GDestroyNotify)gen_cnode_free_callback);

    /* Start message receive thread.  Parent thread becomes
     * responsible for handling inline callbacks */
    state->msgThread = g_thread_create( (GThreadFunc)gen_cnode_handle_connection,
                                        (gpointer)state, 
                                        TRUE,
                                        NULL );
    if( !state->msgThread ){
        rc = -1;
        fprintf( stderr, "Failed to create message receive thread!\n" );
        goto gen_cnode_init_exit;
    }

    state->running = TRUE;
    
    gen_cnode_init_exit:
    return rc;
}

void gen_cnode_exit( gen_cnode_state_t* state ){
  
    if( !state ){
        return;
    }
   
    //Wait for msg thread to finish up
    if( state->msgThread ){
        g_thread_join( state->msgThread );
    }

    if( state->worker_pool ){
        //Wait for our conneciton pool to finish
        g_thread_pool_free( state->worker_pool, TRUE, TRUE );
    }

    if( state->parentQueue ){
        g_async_queue_unref( state->parentQueue );
    }

    g_free(state);
}

void gen_cnode_free_callback( gen_cnode_callback_t* callback ) {

    if( !callback ){
        return;
    }

    if( callback->msg ){
        g_free(callback->msg);
    }

    g_free(callback);
}

bool gen_cnode_msg2cb( char* msg, 
                       uint32_t len,  
                       gen_cnode_callback_t* callback ){
    bool isvalid = true;
    int rc = 0;
    int index = 0;
    int arity = 0; 
    int version = 0;
    char actor [256];

    if( !msg || !len || !callback ){
        isvalid = false;
        goto msg2cb_exit;
    } 
   
    //Decode magic version 
    if( (rc = ei_decode_version(msg, &index, &version)) ){
        fprintf(stderr, "NO VERISON! rc = %d\n", rc);
        isvalid = false;
        goto msg2cb_exit;
    }

    //All messages must be of the form {{parent,worker}, pid, Callback}
    //where pid is an erlang pid and Callback is a 3 arity
    //tuple. 
    if( ei_decode_tuple_header(msg, &index, &arity) ){
        fprintf(stderr, "Failed to decode tuple! Msg must be a tuple!");
        isvalid = false;
        goto msg2cb_exit;
    }

    if( arity != 3 ){
        fprintf(stderr, "Invalid tuple length! Got %d!\n", arity);
        isvalid = false;
        goto msg2cb_exit;
    }

    //Decode type atom
    memset(actor, 0x00, sizeof(actor));
    if( ei_decode_atom(msg, &index, actor) ){
        fprintf(stderr, "Failed to decode callback actor!\n");
        isvalid = false;
        goto msg2cb_exit;
    }

    /* Interpret actor option */
    if( !g_strcmp0(actor, "parent") ){
       callback->parent = TRUE; 
    } else if( !g_strcmp0(actor, "worker") ){
        callback->parent = FALSE;
    } else {
        fprintf(stderr, "Unsupported callback actor!\n");
        isvalid = false;
        goto msg2cb_exit;
    }

    //Decode from pid
    if( ei_decode_pid(msg, &index, &(callback->from)) ){
        fprintf(stderr, "Failed to decode from pid!\n");
        isvalid = false;
        goto msg2cb_exit;
    }

    //Callbacks must be of the form { lib, func, [args] }.  
    if( ei_decode_tuple_header(msg, &index, &arity) ){
        fprintf(stderr, "Failed to decode tuple! Msg must be a tuple!");
        isvalid = false;
        goto msg2cb_exit;
    }

    //Ensure arity of received callback is 3.
    if( arity != 3 ){
        fprintf(stderr, "Invalid tuple length! Got %d!\n", arity);
        isvalid = false;
        goto msg2cb_exit;
    }

    //Try to decode lib tuple
    if( ei_decode_atom(msg, &index, callback->lib) ){
        fprintf(stderr, "Unable to decode library atom!\n");
        isvalid = false;
        goto msg2cb_exit;
    }

    //Attempt to decode func tuple
    if( ei_decode_atom(msg, &index, callback->func) ){
        fprintf(stderr, "Unable to decode function atom!\n");
        isvalid = false;
        goto msg2cb_exit;
    }

    //Decode the 
    if( ei_decode_list_header(msg, &index, &arity) ){
        fprintf(stderr, "Unable to decode argument list!\n");
        isvalid = false;
        goto msg2cb_exit;
    }

    //Save where we left off so functions can parse args
    callback->argc = arity;
  
    //Set argv pointer to first argument  
    if( arity ){
        callback->argv = msg + index;
    } else {
        callback->argv = NULL;
    }

    //Store the original message.
    callback->msg = msg;

    msg2cb_exit:
    return isvalid;
}

int gen_cnode_handle_connection( gen_cnode_state_t* state ){
    int rc = 0;
    ErlConnect info;
    GError* error = NULL;

    //Reference the inline queue
    g_async_queue_ref( state->parentQueue );
    state->receiving = true;

    while( state->running ){

        //Block and wait for incoming erlang connections....
        state->erl_fd = gen_cnode_net_erl_connect( state->node.fd, 
                                                   &(state->node.ec), 
                                                   &info );
        if( state->erl_fd < 0 ){
            fprintf( stderr, "Bad file-descriptor!\n");
            continue;
        }

        //Enter tight message handling loop
        while( state->receiving ){
            guint32 num_bytes = 0;
            ei_x_buff xbuffer;
            erlang_msg erl_msg;
            gen_cnode_callback_t* callback = NULL;

            memset(&xbuffer, 0x00, sizeof(ei_x_buff));
            if( (rc = ei_x_new(&xbuffer)) ){
                
                fprintf(stderr, "Failed to allocate xbuffer!\n");
                goto handle_connection_exit;
            }
            
            num_bytes = ei_xreceive_msg_tmo( state->erl_fd, 
                                             &erl_msg, 
                                             &xbuffer,
                                             1000 );
          
            //Check for error conditions 
            if( num_bytes == ERL_ERROR ){
                
                switch(erl_errno){
                    
                    case EAGAIN:
                    case ETIMEDOUT:
                        ei_x_free(&xbuffer);
                        continue;

                    default:
                        rc = erl_errno;
                        fprintf(stderr, "ei_xreceive_msg_tmo failed! erl_errno = %d!\n", erl_errno);
                        goto handle_connection_exit;
                }
            }

            //Handle messages based on type
            switch( erl_msg.msgtype ){

                
                case ERL_SEND:      //Regular message, continue on...
                case ERL_REG_SEND:
                    break;

                case ERL_LINK:      //<<HERE>> Investigate further
                case ERL_UNLINK:
                    ei_x_free(&xbuffer);
                    printf("DEBUG: LINK/UNLIK!\n");
                    continue;

                case ERL_TICK:  //Heartbeat
                    ei_x_free(&xbuffer);
                    continue;
                
                case ERL_ERROR: //Something bad happened..timeout, etc...
                    rc = -num_bytes;
                    fprintf( stderr, "Error message received!\n" );
                    goto handle_connection_exit;

                case ERL_EXIT:  //Link to erlang side broken, exit normally
                    state->running = state->receiving = FALSE;
                    continue;

                default:
                    fprintf( stderr, "HUH?!\n" );
                    ei_x_free(&xbuffer);
                    continue;
            }
        
            //Attempt to convert message into a callback
            callback = g_new0( gen_cnode_callback_t, 1 );
            if( !gen_cnode_msg2cb( xbuffer.buff, num_bytes, callback ) ){
                ei_x_free(&xbuffer);
                gen_cnode_free_callback(callback);
                continue;
            }

            fprintf( stderr, "lib: %s, func: %s, argc: %d\n", 
                     callback->lib, callback->func, callback->argc );

            /* If callback is to be handled by worker pool and 
             * such a pool exists, hand callback to workers. */
            if( !callback->parent && state->worker_pool ){
                
                g_thread_pool_push( state->worker_pool, (gpointer)callback, &error );
                if( error ){
                    rc = -1;
                    fprintf( stderr, "g_thread_pool_push failed!\n");
                    fprintf( stderr, "%s\n", error->message);
                    break;
                }
            } else { //Otherwise, hand callback to the parent for inline processing

                g_async_queue_push( state->parentQueue, callback );
            }
        }  
    }

    handle_connection_exit: 
    
    //Unreference inline queue
    g_async_queue_unref( state->parentQueue );
      
    state->receiving = false;

    return rc; 
}


/* erlang message helper function.  Probably should be broken up...*/
void gen_cnode_handle_callback( gen_cnode_callback_t* callback, 
                                gen_cnode_state_t* state ){
    int rc = 0;
    ei_x_buff resp = {NULL};

    ei_x_new(&resp);

    gen_cnode_module_callback( callback, state->modules, &resp );

    if( resp.index ){
      
        rc = ei_send( state->erl_fd, &(callback->from), resp.buff, resp.buffsz);
        if( rc < 0 ){
            fprintf( stderr, "ei_send failed!" );
        }
    }

    ei_x_free(&resp);
    
    gen_cnode_free_callback( callback );
}


/*********** Built-in Functions *************/
int gen_cnode_load( int argc, 
                    char* args, 
                    gen_cnode_state_t* state, 
                    ei_x_buff* resp )
{
    return gen_cnode_module_load(argc, args, state->modules, resp);
}

int gen_cnode_stop( int argc, 
                    char* args, 
                    gen_cnode_state_t* state, 
                    ei_x_buff* resp )
{
    state->running = state->receiving = FALSE;
    return 0;
}
