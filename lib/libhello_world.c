#include <stdio.h>
#include <glib.h>

#include "gen_cnode.h"

GEN_CNODE_REQUIRE("foo");

typedef GEN_CNODE_STATE {
    int call_cnt;
} hw_state_t;

GEN_CNODE_STATE_NEW() {
    return g_new0( hw_state_t, 1 );
}

GEN_CNODE_EXPORT( hello_world );

GEN_CNODE_DEFINE_INIT() {
    printf( "Initializing!\n");
    return 0;
}

GEN_CNODE_DEFINE_EXIT() {
    printf( "Exiting!\n" );
    return 0;
}

GEN_CNODE_DEFINE( hello_world ){
    printf( "Hello World!\n" );
    *resp = erl_format("~s", "Hello World!");
    return (int)0;
}
