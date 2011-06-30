/**
 * @file:  acorn_net.h
 * @breif: Acorn  c_node netowrk utility library.
 */
#ifndef GEN_CNODE_NET_H
#define GEN_CNODE_NET_H

#include <string.h>
#include <glib.h>

/* Networking bits */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "erl_interface.h"
#include "ei.h"

typedef struct gen_cnode_s {
    int fd;
    struct sockaddr_in addr;
    ei_cnode ec;
} gen_cnode_t;

/* Binds a fd to localhost:<port> and performs necessary setup work for epmd */
int gen_cnode_net_init( char* name,
                        char* host, 
                        char* secret,
                        uint32_t port, 
                        uint16_t creation,
                        gen_cnode_t* node ); 

/* Listens for a connection for an erlang process 
 * and populates the pssed ErlConnect data structure.
 * <<HERE>> Ensure the connecting erlang node is proper? */
int gen_cnode_net_erl_connect( int listen_fd, 
                               ei_cnode* node, 
                               ErlConnect* conn ); 

#endif
