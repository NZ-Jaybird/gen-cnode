#include "gen_cnode_net.h"

/* Helper function which opens a stream sockets, binds it to a particular port,
 * and outputs the resulting file descriptor. */
int gen_cnode_net_bind( struct sockaddr_in *addr, int* fd ){
    int rc = 0;
    int on = 1;

    if( !fd ){
        rc = -EINVAL;
        goto gen_cnode_net_bind_exit;
    }

    //Fetch a socket fd 
    *fd = socket( AF_INET, SOCK_STREAM, 0 );
    if( *fd < 0 ){
        fprintf( stderr, "socket creation failed! rc = %d\n", rc);
        goto gen_cnode_net_bind_exit;
    }

    //Set socket options
    setsockopt( *fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on) );
    
    //Perform the acutal binding
    rc = bind( *fd, (struct sockaddr*) addr, sizeof(*addr) );
    if( rc < 0 ){
        *fd = -1;
        fprintf( stderr, "bind failed! rc = %d\n",rc );
        goto gen_cnode_net_bind_exit;
    }

    //Begin listening on the file descriptor
    rc = listen( *fd, 5 );
    if( rc ){
        *fd = -1;
        fprintf( stderr, "listen failed! rc = %d\n", rc );
    }

    gen_cnode_net_bind_exit:
    return rc;    
}

int gen_cnode_net_init( uint32_t id, 
                        char* secret,
                        uint16_t creation,
                        uint32_t port, 
                        struct sockaddr_in *addr,
                        int *fd ){
    int rc = 0;

    if( !addr || !fd ){
        rc = -EINVAL;
        goto gen_cnode_net_init_exit;
    }

    //<<HERE>> Setup the port on localhost
    memset( addr, 0x00, sizeof(*addr) );
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    addr->sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    rc = gen_cnode_net_bind( addr, fd );
    if( rc ){
        fprintf( stderr, "gen_cnode_net_bind failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    //Tell erlang about ourselves
    rc = erl_connect_init( id, secret, creation );
    if( rc == -1 ){
        fprintf( stderr, "erl_connect_init failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    //Publish our port to epmd ( assumes epmd is running! )
    rc = erl_publish( port );
    if( rc == -1 ){
        fprintf( stderr, "erl_publish failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    rc = 0;

    gen_cnode_net_init_exit:
    return rc;
}

//<<HERE>> Currently not implemented
int gen_cnode_net_xinit( char* node_name,
                         char* fqdn, 
                         char* secret, 
                         uint16_t creation,
                         int port,  
                         struct sockaddr_in *output ){
    return 0;
}

int gen_cnode_net_erl_connect( int listen_fd, ErlConnect* conn ){
    int erl_fd = -1;
    
    erl_fd = erl_accept( listen_fd, conn );
    if( erl_fd == ERL_ERROR ){
        fprintf( stderr, "erl_accept failed! rc = ERL_ERROR\n" );
        goto gen_cnode_net_erl_connect_exit;
    }

    printf( "Connected to: %s\n", conn->nodename );

    gen_cnode_net_erl_connect_exit:
    return erl_fd;
}
