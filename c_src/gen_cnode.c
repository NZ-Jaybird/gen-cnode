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
    gen_cnode_t node;           //Node network specifics: fd, addr_in, ei_cnode
    gen_cnode_opts_t* opts;     //gen_cnode options
    GHashTable* modules;        //Library hashtable
    GThreadPool* callback_pool; //Thread pool reserved for handling callback
} gen_cnode_state_t;

int gen_cnode_check_args( gen_cnode_opts_t* opts );
int gen_cnode_init( gen_cnode_opts_t* opts, gen_cnode_state_t* state );
int gen_cnode_handle_connection( gen_cnode_state_t* state );
void gen_cnode_handle_callback( gen_cnode_callback_t* callback, 
                                gen_cnode_state_t* state );
void gen_cnode_exit(gen_cnode_state_t* state);

int main( int argc, char** argv ){
    int rc = 0;
    ErlConnect info;
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
        fprintf( stderr, "Failed to parse CLAs!\n" );
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

    while( state->running ){

        //Block and wait for incoming erlang connections....
        state->erl_fd = gen_cnode_net_erl_connect( state->node.fd, 
                                                   &(state->node.ec), 
                                                   &info );
        if( state->erl_fd < 0 ){
            fprintf( stderr, "Bad file-descriptor!\n");
            continue;
        }

        //Enter tight msg handling loop
        gen_cnode_handle_connection( state );
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
        fprintf( stderr, "You must specify a node name (-N)! "
                         "Try --help\n");
        goto check_args_exit;
    }

    if( opts->port == 0 ){
        rc = -EINVAL;
        fprintf( stderr, "You must specify a non-zero port (-P)! "
                 "Try --help-all\n");
    }

    if( opts->threads < 0 ){
        rc = -EINVAL;
        fprintf( stderr, "Number of worker threads must be non-zero!");
    }

    check_args_exit:
    return rc;
}


int gen_cnode_init( gen_cnode_opts_t* opts, gen_cnode_state_t* state ){
    int rc = 0;

    //Setup GLIB threading
    if( !g_thread_supported() ){
        g_thread_init(NULL);
    }

    //Setup connection pool
    state->callback_pool = g_thread_pool_new( (GFunc)gen_cnode_handle_callback,
                                              state, opts->threads, FALSE, FALSE );
    if( !(state->callback_pool) ){
        rc = -1;
        fprintf( stderr, "g_thread_pool_new failed!\n" );
        goto gen_cnode_init_exit;
    }

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

    state->running = TRUE;
    state->modules = gen_cnode_module_init();

    gen_cnode_init_exit:
    return rc;
}

void gen_cnode_exit( gen_cnode_state_t* state ){
  
    if( !state ){
        return;
    }

    if( state->callback_pool ){
        //Wait for our conneciton pool to finish
        g_thread_pool_free( state->callback_pool, TRUE, TRUE );
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

    //All messages must be of the form {pid, Callback}
    //where pid is an erlang pid and Callback is a 3 arity
    //tuple. 
    if( ei_decode_tuple_header(msg, &index, &arity) ){
        fprintf(stderr, "Failed to decode tuple! Msg must be a tuple!");
        isvalid = false;
        goto msg2cb_exit;
    }

    if( arity != 2 ){
        fprintf(stderr, "Invalid tuple length! Got %d!\n", arity);
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
    bool receiving = TRUE;
    GError* error = NULL;

    while( receiving ){
        guint32 num_bytes = 0;
        ei_x_buff xbuffer;
        erlang_msg erl_msg;
        gen_cnode_callback_t* callback = NULL;

        memset(&xbuffer, 0x00, sizeof(ei_x_buff));
        if( (rc = ei_x_new(&xbuffer)) ){
            
            fprintf(stderr, "Failed to allocate xbuffer!\n");
            goto handle_connection_exit;
        }
        
        num_bytes = ei_receive_msg( state->erl_fd, 
                                    &erl_msg, 
                                    &xbuffer );
        
        //<<HERE>> Pull out EMSGSIZE and allow for variable length? 
        switch( num_bytes ){
            case EAGAIN:
            case ERL_TICK:
                continue;
            
            case EMSGSIZE:
            case ENOMEM:
            case EIO:
                rc = -num_bytes;
                fprintf( stderr, "Buffer I/O error! rc = %d\n", num_bytes );
                goto handle_connection_exit;
            
            case ERL_ERROR:
                rc = -num_bytes;
                fprintf( stderr, "ei_receive_msg failed! rc = %d\n", rc );
                goto handle_connection_exit;

            default:
                break;
        }
    
        //Only accept ERL_REG_SEND 
        if( erl_msg.msgtype != ERL_REG_SEND ){
            ei_x_free(&xbuffer);
            fprintf( stderr, "Only ERL_REG_SEND is supported!\n");
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

        //<<HERE>> Better way to handle this?
        if( !(g_strcmp0(callback->lib, "gen_cnode")) && 
            !(g_strcmp0(callback->func, "stop")) )
        {
                state->running = FALSE;
                receiving = FALSE;
                gen_cnode_free_callback( callback );
                break;
        }
        
        //Otherwise, push the request into the pool and get back to listening..
        g_thread_pool_push( state->callback_pool, (gpointer)callback, &error );
        if( error ){
            rc = -1;
            fprintf( stderr, "g_thread_push failed!\n");
            break;
        } 
    }  
   
    handle_connection_exit: 
    return rc; 
}


/* erlang message helper function.  Probably should be broken up...*/
void gen_cnode_handle_callback( gen_cnode_callback_t* callback, 
                                gen_cnode_state_t* state ){
    int rc = 0;
    ei_x_buff* resp = NULL;

    gen_cnode_module_callback( callback, state->modules, &resp );

    if( resp ){
       
        rc = ei_send( state->erl_fd, &(callback->from), resp->buff, resp->buffsz);
        if( rc < 0 ){
            fprintf( stderr, "ei_send failed!" );
        }

        ei_x_free(resp);
    }

    gen_cnode_free_callback( callback );
}
