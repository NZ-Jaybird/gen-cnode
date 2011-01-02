include ../Makedefs

LIB_TARGETS := libhello_world.so

CFLAGS += -I../include
CFLAGS += -I$(ERL_INTERFACE_INCLUDE)
CFLAGS += $(shell pkg-config --cflags glib-2.0)

ifeq ($(strip $(KERNEL)),Darwin)
LDFLAGS := -Wall -fPIC -shared -flat_namespace -undefined suppress 
else
LDFLAGS := -Wall -fPIC -shared -flat_namespace -undefined 
endif

default: all ;

all: $(LIB_TARGETS) ;

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(LIB_TARGETS)

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<

libhello_world.so: CFLAGS += -fPIC
libhello_world.so: libhello_world.o
	$(CC) $(LDFLAGS) $^ $(LFLAGS) -o $@
