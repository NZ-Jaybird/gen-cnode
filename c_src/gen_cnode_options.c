#include "gen_cnode_options.h"

gen_cnode_opts_t gen_cnode_opts = { 
    .name = NULL,
    .port = 0, 
    .cookie = NULL,
    .creation = 0 
};

gchar gen_cnode_opt_sname[] = "gen_cnode common";
gchar gen_cnode_opt_lname[] = "gen_cnode common options:";
gchar gen_cnode_opt_shows[] = "Show gen_cnode common options";
GOptionEntry gen_cnode_opt_entries[] = {
    GEN_CNODE_NAME,
    GEN_CNODE_PORT,
    GEN_CNODE_COOKIE,
    GEN_CNODE_CREATION,
    {NULL}
};



