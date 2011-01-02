#include <stdio.h>
#include <stdbool.h>
#include <glib.h>

#include "gen_cnode.h"
#include "gen_cnode_options.h"      //Command line options
//#include "gen_cnode_msg.h"          //Internal message types
#include "gen_cnode_net.h"          //Net utilities
#include "gen_cnode_module.h"       //Module support

#include "erl_interface.h"
#include "ei.h"

#define GEN_CNODE_RECV_MAX 4096 //4KB of data max

typedef struct gen_cnode_state_s {
    int listen_fd;              //FD for incoming messages
    int erl_fd;                 //FD for outgoing messages
    struct sockaddr_in addr;
    gboolean running;           //Have we received a stop commmand?
    gen_cnode_opts_t* opts;     //gen_cnode options
    GHashTable* modules;        //Library hashtable
    GThreadPool* callback_pool; //Thread pool reserved for handling callback
} gen_cnode_state_t;

/* Erlang connection information.  Handed to handle_connection. */
typedef struct gen_cnode_conn_s {
    int fd;
    ErlConnect info;
} gen_cnode_conn_t;

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
        state->erl_fd = gen_cnode_net_erl_connect( state->listen_fd, &info );
        if( state->erl_fd < 0 ){
            fprintf( stderr, "Bad file-descriptor!\n");
            continue;
        }

        //Enter tight msg handling loop
        gen_cnode_handle_connection( state );
    }

    main_exit:
    gen_cnode_exit( state );
    return rc;
}

int gen_cnode_check_args( gen_cnode_opts_t* opts ){
    int rc = 0;

    if( opts->port == 0 ){
        rc = -EINVAL;
        fprintf( stderr, "You must specify a non-zero port (-P)! "
                 "Try --help-all\n");
    }

    return rc;
}


int gen_cnode_init( gen_cnode_opts_t* opts, gen_cnode_state_t* state ){
    int rc = 0;

    //rc = gen_cnode_parse_options( state );

    //Reqired by ei/erl_interface
    erl_init( NULL, 0 );

    //Setup GLIB threading
    if( !g_thread_supported() ){
        g_thread_init(NULL);
    }

    //Setup connection pool
    state->callback_pool = g_thread_pool_new( (GFunc)gen_cnode_handle_callback,
                                              state, 50, FALSE, FALSE );
    if( !(state->callback_pool) ){
        rc = -1;
        fprintf( stderr, "g_thread_pool_new failed!\n" );
        goto gen_cnode_init_exit;
    }

    rc = gen_cnode_net_init( opts->id, 
                             opts->cookie, 
                             0, opts->port, 
                             &(state->addr), 
                             &(state->listen_fd) );
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
    
}

void gen_cnode_free_callback( gen_cnode_callback_t* callback ) {
    erl_free_term( callback->args );
    g_free( callback->lib );
    g_free( callback->func );
    
    g_free( callback );
}

void gen_cnode_emsg_destroy( ErlMessage* emsg ){
    
    if( !emsg ) {
        return;
    }

    if( emsg->msg )
        erl_free_term( emsg->msg );
    
    if( emsg->from )
        erl_free_term( emsg->from );
    
    if( emsg->to )    
        erl_free_term( emsg->to );
}

bool gen_cnode_msg2cb( ErlMessage* emsg, gen_cnode_callback_t* callback ){
    bool isvalid = true;
    ETERM* msg;
    ETERM* lib_term = NULL;
    ETERM* func_term = NULL;
    ETERM *args = NULL;

    msg = erl_element( 3, emsg->msg );    

    lib_term = erl_element( 1, msg );
    func_term = erl_element(2, msg);
    args = erl_element(3, msg);

    if( !lib_term || !func_term || !args || !(ERL_IS_LIST(args)) ){
        isvalid = false;
        goto gen_cnode_msg2cb_exit;
    }

    callback->lib = ERL_ATOM_PTR( lib_term );
    callback->func = ERL_ATOM_PTR( func_term );
    callback->args = args;
    callback->from_pid = erl_element( 2, emsg->msg );

    erl_free_term( lib_term );
    erl_free_term( func_term );
    
    gen_cnode_msg2cb_exit:
    return isvalid;
}

int gen_cnode_handle_connection( gen_cnode_state_t* state ){
    int rc = 0;
    bool receiving = TRUE;
    gen_cnode_callback_t* callback = NULL;
    uint8_t recv_buff [GEN_CNODE_RECV_MAX];
    uint32_t num_bytes = 0;
    GError* error = NULL;

    while( receiving ){
        ErlMessage emsg;

        //Cleanup after last run
        memset( &emsg, 0x00, sizeof(emsg) );

        num_bytes = erl_receive_msg( state->erl_fd, 
                                     recv_buff, 
                                     GEN_CNODE_RECV_MAX,
                                     &emsg );
        
        //<<HERE>> Pull out EMSGSIZE and allow for variable length? 
        switch( num_bytes ){
            case EMSGSIZE:
            case ENOMEM:
            case EIO:
                rc = -num_bytes;
                fprintf( stderr, "Buffer I/O error! rc = %d\n", num_bytes );
                goto gen_cnode_handle_connection_exit;
            
            case ERL_TICK:
                gen_cnode_emsg_destroy( &emsg );
                continue;

            case ERL_ERROR:
                rc = -num_bytes;
                fprintf( stderr, "erl_receive_msg failed! rc = %d\n", rc );
                goto gen_cnode_handle_connection_exit;

            default:
                break;
        }
        
        //If the message isn't a regular transmission, ignore it
        if( emsg.type != ERL_REG_SEND ){
            fprintf( stderr, "Received unsupported message type!\n" );
            gen_cnode_emsg_destroy( &emsg );
            continue;
        }

        //Attempt to convert message into a callback
        callback = g_new0( gen_cnode_callback_t, 1 );
        if( !gen_cnode_msg2cb( &emsg, callback ) ){
            gen_cnode_emsg_destroy( &emsg );
            continue;
        }

        fprintf( stderr, "lib: %s, func: %s\n", callback->lib, callback->func );
        gen_cnode_emsg_destroy( &emsg );

        //<<HERE>> Better way to handle this?
        if( !(g_strcmp0(callback->lib, "gen_cnode")) && 
            !(g_strcmp0(callback->func, "stop")) )
        {
                state->running = FALSE;
                receiving = FALSE;
                gen_cnode_free_callback( callback );
                break;
        }

        /*//Otherwise, push the request into the pool and get back to listening..
        g_thread_pool_push( state->callback_pool, (gpointer)callback, &error );
        if( error ){
            rc = -1;
            fprintf( stderr, "g_thread_push failed!\n");
            break;
        } 

        */
    }  
   
    gen_cnode_handle_connection_exit: 
    return rc; 
}


/* erlang message helper function.  Probably should be broken up...*/
void gen_cnode_handle_callback( gen_cnode_callback_t* callback, 
                                gen_cnode_state_t* state ){
    int rc = 0;
    ETERM* resp = NULL;

    rc = gen_cnode_module_callback( callback, state->modules, &resp );
    if( rc ){
        fprintf( stderr, "gen_cnode_module_callback failed! rc = %d", rc );
        goto gen_cnode_handle_callback_exit;
    }

    if( resp ){
        
        if( !(rc = erl_send( state->erl_fd, callback->from_pid, resp)) ){
            printf( "erl_send failed!");    
        } 
        erl_free_term( resp );
    }
   
    gen_cnode_handle_callback_exit: 
    gen_cnode_free_callback( callback );
}
