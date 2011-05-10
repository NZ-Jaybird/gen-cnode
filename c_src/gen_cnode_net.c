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
                        gen_cnode_t* node){
    int rc = 0;

    if( !node || !name ){
        rc = -EINVAL;
        goto gen_cnode_net_init_exit;
    }

    //<<HERE>> Setup the port on localhost
    memset( &(node->addr), 0x00, sizeof(node->addr) );
    node->addr.sin_family = AF_INET;
    node->addr.sin_port = htons(port);
    node->addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    rc = gen_cnode_net_bind( node );
    if( rc ){
        fprintf( stderr, "gen_cnode_net_bind failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    rc = ei_connect_init( &(node->ec), name, secret, creation );
    if( rc < 0 ){
        fprintf( stderr, "ei_connect_init failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    //Publish our port to epmd ( assumes epmd is running! )
    rc = ei_publish( &(node->ec), port );
    if( rc < 0 ){
        fprintf( stderr, "ei_publish failed! rc = %d\n", rc );
        goto gen_cnode_net_init_exit;
    }

    

    rc = 0;

    gen_cnode_net_init_exit:
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
