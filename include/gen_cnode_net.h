/**
 * @file:  acorn_net.h
 * @breif: Acorn  c_node netowrk utility library.
 */
#ifndef GEN_CNODE_NET_H
#define GEN_CNODE_NET_H

#include <string.h>

/* Networking bits */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "erl_interface.h"
#include "ei.h"


/* Binds a fd to localhost:<port> and performs necessary setup work for epmd */
int gen_cnode_net_init( uint32_t id, 
                        char* secret,
                        uint16_t creation,
                        uint32_t port, 
                        struct sockaddr_in *output,
                        int *fd ); 

//<<HERE>> Currently unimplemented!
int gen_cnode_net_xinit( char* node_name,
                         char* fqdn, 
                         char* secret, 
                         uint16_t creation,
                         int port,  
                         struct sockaddr_in *output );


/* Listens for a connection for an erlang process 
 * and populates the pssed ErlConnect data structure.
 * <<HERE>> Ensure the connecting erlang node is proper? */
int gen_cnode_net_erl_connect( int listen_fd, ErlConnect* conn ); 

#endif
