#ifndef GEN_CNODE_H
#define GEN_CNODE_H

#include "erl_interface.h"
#include "ei.h"

//Forward declare lib state object (
struct gen_cnode_lib_state_s;

typedef int (*gen_cnode_fp) ( ETERM* args, 
                              ETERM** resp, 
                              struct gen_cnode_lib_state_s *state);

typedef struct gen_cnode_lib_state_s * (*gen_cnode_state_new_fp) ();
typedef int (*gen_cnode_init_fp) ( struct gen_cnode_lib_state_s *lib_state );
typedef int (*gen_cnode_exit_fp) ();

#define GEN_CNODE_REQUIRE(LIBS)                                              \
static char* GEN_CNODE_REQUIRED[] __attribute__ (( unused )) = {LIBS, NULL}

#define GEN_CNODE_STATE()                                               \
struct gen_cnode_lib_state_s                                                \

#define GEN_CNODE_STATE_NEW()                                               \
struct gen_cnode_lib_state_s* GEN_CNODE_STATE_NEW()                     \

#define GEN_CNODE_STATE_FREE()                                          \
void GEN_CNODE_STATE_FREE( struct gen_cnode_lib_state_s *lib_state )    \

#define GEN_CNODE_DEFINE_INIT()                                             \
int GEN_CNODE_INIT( struct gen_cnode_lib_state_s *state )                   \

#define GEN_CNODE_DEFINE_EXIT( )                        \
int GEN_CNODE_EXIT()                                    \

//Function definition macro...used to assert type-safety
#define GEN_CNODE_EXPORT( NAME )                                              \
int NAME( ETERM* args, ETERM** resp, struct gen_cnode_lib_state_s * state ); \
gen_cnode_fp GEN_CNODE_##NAME __attribute__ (( unused )) = NAME;       \

#define GEN_CNODE_DEFINE( NAME )                    \
int NAME( ETERM* args, ETERM** resp, struct gen_cnode_lib_state_s *lib_state )   \

#endif
