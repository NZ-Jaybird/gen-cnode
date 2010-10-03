#Import global variables
include ../Makedefs

BIN_TARGETS := gen_cnode 
LIB_TARGETS := 

LCFLAGS += -I../include
LCFLAGS += -I$(ERL_INTERFACE_INCLUDE)
LCFLAGS += $(shell pkg-config --cflags glib-2.0)
LCFLAGS += $(shell pkg-config --cflags gthread-2.0)

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
	$(CC) -c $(LCFLAGS) -o $@ $(CFLAGS) $<

gen_cnode: gen_cnode_module.o gen_cnode_net.o gen_cnode_options.o gen_cnode.o
	$(CC) $(LDFLAGS) $^ -o $@
