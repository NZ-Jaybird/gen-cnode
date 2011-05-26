#include "gen_cnode_options.h"

gen_cnode_opts_t gen_cnode_opts = { NULL, 0, 2, 0, NULL };

gchar gen_cnode_opt_sname[] = "gen_cnode common";
gchar gen_cnode_opt_lname[] = "gen_cnode common options:";
gchar gen_cnode_opt_shows[] = "Show gen_cnode common options";
GOptionEntry gen_cnode_opt_entries[] = {
    GEN_CNODE_NAME,
    GEN_CNODE_PORT,
    GEN_CNODE_THREADS,
    GEN_CNODE_CREATION,
    GEN_CNODE_COOKIE,
    {NULL}
};
