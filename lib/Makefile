include ../Makedefs

LIB_TARGETS := libhello_world.so

LCFLAGS += -I../include
LCFLAGS += -I$(ERL_INTERFACE_INCLUDE)
LCFLAGS += $(shell pkg-config --cflags glib-2.0)

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
