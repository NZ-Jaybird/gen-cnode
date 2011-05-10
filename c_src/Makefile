#Import global variables
include ../Makedefs

BIN_TARGETS := gen_cnode 
LIB_TARGETS := 

CFLAGS += -I../include
CFLAGS += -I$(ERL_INTERFACE_INCLUDE)
CFLAGS += $(shell pkg-config --cflags glib-2.0)
CFLAGS += $(shell pkg-config --cflags gthread-2.0)

LDFLAGS += -L$(ERL_INTERFACE_LIB) -lerl_interface -lei
LDFLAGS += $(shell pkg-config --libs glib-2.0)
LDFLAGS += $(shell pkg-config --libs gthread-2.0)
LDFLAGS += $(shell pkg-config --libs gmodule-2.0)

default: all ;

all: $(BIN_TARGETS) $(LIB_TARGETS) ;

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(BIN_TARGETS)

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

gen_cnode: gen_cnode_net.o gen_cnode_options.o gen_cnode.o
	$(CC) $^ $(LDFLAGS) -o $@
