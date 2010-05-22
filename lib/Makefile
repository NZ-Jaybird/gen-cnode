CC=gcc
CFLAGS= -g -Wall

LIB_TARGETS := libhello_world.so

LCFLAGS += -I/usr/local/lib/erlang/lib/erl_interface-3.6.3/include
LCFLAGS += $(shell pkg-config --cflags glib-2.0)
LCFLAGS += -I../include

LDFLAGS += -Wall -fPIC -shared -flat_namespace -undefined
LDFLAGS += $(shell pkg-config --libs glib-2.0)
LDFLAGS += -L/usr/local/lib/erlang/lib/erl_interface-3.6.3/lib
LFLAGS := -lerl_interface -lei

default: all ;

all: $(LIB_TARGETS) ;

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(LIB_TARGETS)

%.o: %.c
	$(CC) -c $(LCFLAGS) -o $@ $(CFLAGS) $<

libhello_world.so: LCFLAGS += -fPIC
libhello_world.so: libhello_world.o
	$(CC) $(LDFLAGS) $^ $(LFLAGS) -o $@
