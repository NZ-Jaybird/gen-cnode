#include "gen_cnode_net.h"

/* Helper function which opens a stream sockets, binds it to a particular port,
 * and outputs the resulting file descriptor. */
int gen_cnode_net_bind( gen_cnode_t* node ){
    int rc = 0;
    int on = 1;

    if( !node ){
        rc = -EINVAL;
        goto gen_cnode_net_bind_exit;
    }

    //Fetch a socket fd 
    node->fd = socket( AF_INET, SOCK_STREAM, 0 );
    if( node->fd < 0 ){
        fprintf( stderr, "socket creation failed! rc = %d\n", rc);
        goto gen_cnode_net_bind_exit;
    }

    //Set socket options
    setsockopt( node->fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on) );
    
    //Perform the acutal binding
    rc = bind( node->fd, (struct sockaddr*) &(node->addr), sizeof(node->addr) );
    if( rc < 0 ){
        node->fd = -1;
        fprintf( stderr, "bind failed! rc = %d\n",rc );
        goto gen_cnode_net_bind_exit;
    }

    //Begin listening on the file descriptor
    rc = listen( node->fd, 5 );
    if( rc ){
        node->fd = -1;
        fprintf( stderr, "listen failed! rc = %d\n", rc );
    }

    gen_cnode_net_bind_exit:
    return rc;    
}

int gen_cnode_net_init( char* name, 
                        char* secret,
                        uint32_t port,
                        uint16_t creation,
                        gen_cnode_t* state){
    int rc = 0;
    const char* hostname = "localhost";
    char* node = NULL;

    if( !state || !name ){
        rc = -EINVAL;
        goto gen_cnode_net_init_exit;
    }

    //Setup networking info
    memset( &(state->addr), 0x00, sizeof(state->addr) );
    state->addr.sin_family = AF_INET;
    state->addr.sin_port = htons(port);
    state->addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    //Bind to localhost:port
    rc = gen_cnode_net_bind( state );
    if( rc ){
        fprintf( stderr, "gen_cnode_net_bind failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    node = g_strdup_printf("%s@%s", name, hostname);

    //Connect to erlang runtime 
    rc = ei_connect_xinit( &(state->ec),
                           hostname,
                           name,
                           node, 
                           &(state->addr.sin_addr), 
                           secret,
                           creation );
    if( rc < 0 ){
        fprintf(stderr, "ei_connect_xinit failed! rc = %d", errno);
        goto gen_cnode_net_init_exit;
    } 

    //Publish our port to epmd ( assumes epmd is running! )
    rc = ei_publish( &(state->ec), port );
    if( rc < 0 ){
        fprintf( stderr, "ei_publish failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    rc = 0;

    gen_cnode_net_init_exit:

    if( node ){
        g_free(node);
    }
    return rc;
}

int gen_cnode_net_erl_connect( int listen_fd,
                               ei_cnode* node, 
                               ErlConnect* conn ){
    int erl_fd = -1;
    
    erl_fd = ei_accept( node, listen_fd, conn );
    if( erl_fd == ERL_ERROR ){
        fprintf( stderr, "erl_accept failed! rc = ERL_ERROR\n" );
        goto gen_cnode_net_erl_connect_exit;
    }

    printf( "Connected to: %s\n", conn->nodename );

    gen_cnode_net_erl_connect_exit:
    return erl_fd;
}
