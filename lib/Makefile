include ../Makedefs

LIB_TARGETS := libhello_world.so libhello_world_cpp.so

CFLAGS += -I../include
CFLAGS += -I$(ERL_INTERFACE_INCLUDE)
CFLAGS += $(shell pkg-config --cflags glib-2.0)

ifeq ($(strip $(KERNEL)),Darwin)
LDFLAGS := -Wall -fPIC -shared -flat_namespace -undefined suppress 
else
LDFLAGS := -Wall -fPIC -shared -undefined 
endif

default: all ;

all: $(LIB_TARGETS) ;

install: all;

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(LIB_TARGETS)

libhello_world_cpp.o: libhello_world.cpp
	$(CPP) -c -o $@ $(CFLAGS) $<

libhello_world_cpp.so: CFLAGS += -fPIC
libhello_world_cpp.so: libhello_world_cpp.o
	$(CPP) $(LDFLAGS) $^ $(LFLAGS) -o $@

libhello_world.o: libhello_world.c
	$(CC) -c -o $@ $(CFLAGS) $<

libhello_world.so: CFLAGS += -fPIC
libhello_world.so: libhello_world.o
	$(CC) $(LDFLAGS) $^ $(LFLAGS) -o $@
