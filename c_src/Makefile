CC=gcc
CPP=g++
CFLAGS= -g -Wall

BIN_TARGETS := gen_cnode 
LIB_TARGETS := 

LCFLAGS += -I/usr/local/lib/erlang/lib/erl_interface-3.6.3/include
LCFLAGS += $(shell pkg-config --cflags glib-2.0)
LCFLAGS += $(shell pkg-config --cflags gthread-2.0)
LCFLAGS += -I../include

LDFLAGS += -L/usr/local/lib/erlang/lib/erl_interface-3.6.3/lib
LDFLAGS += $(shell pkg-config --libs glib-2.0)
LDFLAGS += $(shell pkg-config --libs gthread-2.0)
LDFLAGS += $(shell pkg-config --libs gmodule-2.0)

LFLAGS := -lerl_interface -lei

default: all ;

all: $(BIN_TARGETS) $(LIB_TARGETS) ;

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(BIN_TARGETS)

%.o: %.c
	$(CC) -c $(LCFLAGS) -o $@ $(CFLAGS) $<

gen_cnode: gen_cnode_module.o gen_cnode_net.o gen_cnode_options.o gen_cnode.o
	$(CC) $(LDFLAGS) $(shell pkg-config --libs OGRE) $^ $(LFLAGS) -o $@
