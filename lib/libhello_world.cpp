#include <iostream>
#include <glib.h>

#include "gen_cnode.h"

//GEN_CNODE_REQUIRE("foo","bar");

class hw {
    public: 
        std::string speak();
};

std::string hw::speak(){
    return "Hello World From a Class!";
}

int hello_world_cpp(ei_x_buff* resp){
    hw* test = NULL;

    ei_x_format(resp, "~s", "Hello World!");

    test = new hw();
    std::cout << test->speak() << std::endl;
    delete test;
    
    return 0;
}

#ifdef __cplusplus
extern "C" {
#endif

typedef GEN_CNODE_STATE {
    int call_cnt;
} hw_state_t;

GEN_CNODE_STATE_NEW() {
    return (hw_state_t*) g_new0( hw_state_t, 1 );
}

GEN_CNODE_EXPORT( hello_world );

GEN_CNODE_DEFINE_INIT() {
    printf( "Initializing!\n");
    printf( "Calling sync_with_stdio!!\n");
    std::ios::sync_with_stdio();
    return 0;
}

GEN_CNODE_DEFINE_EXIT() {
    printf( "Exiting!\n" );
    return 0;
}

GEN_CNODE_DEFINE( hello_world ){
  
    hello_world_cpp(resp);

    return (int)0;
}

#ifdef __cplusplus
}
#endif


