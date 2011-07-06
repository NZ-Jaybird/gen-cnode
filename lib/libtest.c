#include <stdio.h>
#include <glib.h>

#include "gen_cnode.h"

//GEN_CNODE_REQUIRE("foo","bar");

typedef GEN_CNODE_STATE {
    int call_cnt;
} hw_state_t;

GEN_CNODE_STATE_NEW() {
    return g_new0( hw_state_t, 1 );
}

GEN_CNODE_EXPORT( ping );

GEN_CNODE_DEFINE_INIT() {
    printf( "Initializing!\n");
    return 0;
}

GEN_CNODE_DEFINE_EXIT() {
    printf( "Exiting!\n" );
    return 0;
}

GEN_CNODE_DEFINE( ping ){
    gen_cnode_format(resp, "~s", "pong");
    return 0;
}

GEN_CNODE_DEFINE( event ){
    ei_x_buff event = {0};

    ei_x_new(&event);

    gen_cnode_format(&event, "~s", "Hello World!");

    gen_cnode_notify("hello_world", &event);

    gen_cnode_format(resp, "~a", "ok");

    ei_x_free(&event);
    return 0;
}
