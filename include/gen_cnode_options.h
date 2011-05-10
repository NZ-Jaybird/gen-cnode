#ifndef GEN_CNODE_OPTIONS_H
#define GEN_CNODE_OPTIONS_H

#include <glib.h>

typedef struct gen_cnode_opts_s {
    gchar* name;            //(Require) Name associated with cnode
    guint16 port;           //(Required) Port on localhost on which to communicate
    guint32 threads;        //(Optional) Numer of worker threads
    guint32 creation;       //(Optional) Numeric id of the node for short name mode.
    gchar* cookie;          //(Optional) Cookie used to decrypt/encrypt erlang messages
} gen_cnode_opts_t;

extern gen_cnode_opts_t gen_cnode_opts;

/* CLA Specifications */
#define GEN_CNODE_PORT                                              \
    {                                                               \
        "port", 'P', 0, G_OPTION_ARG_INT, &(gen_cnode_opts.port),   \
        "Port to use for IPC", "<port>"                             \
    }                                                               \

#define GEN_CNODE_CREATION                                          \
    {                                                               \
        "creation", 'c', 0, G_OPTION_ARG_INT,                       \
        &(gen_cnode_opts.creation),                                 \
        "Creation number used for ei_connect_init.",                \
        "<creation>"                                                \
    }                                                               \

#define GEN_CNODE_THREADS                                           \
    {                                                               \
        "threads", 't', 0, G_OPTION_ARG_INT,                        \
        &(gen_cnode_opts.threads),                                  \
        "Number of worker threads for handling callbacks",          \
        "<#threads>"                                                \
    }     

#define GEN_CNODE_NAME                                              \
    {                                                               \
        "name", 'N', 0, G_OPTION_ARG_STRING,                        \
         &(gen_cnode_opts.name),                                    \
        "Long node name used to identify the cnode",                \
        "<node_name>"                                               \
    }                                                               \

#define GEN_CNODE_COOKIE                                            \
    {                                                               \
        "setcookie", 'S', 0, G_OPTION_ARG_STRING,                   \
         &(gen_cnode_opts.cookie),                                  \
        "Secret used to encrypt/decrypt erlang messages",           \
        "<cookie>"                                                  \
    }                                                               \

extern gchar gen_cnode_opt_sname[];
extern gchar gen_cnode_opt_lname[];
extern gchar gen_cnode_opt_shows[];
extern GOptionEntry gen_cnode_opt_entries[];

#endif
