include ../Makedefs

LIB_TARGETS := libtest.so

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

libtest.o: libtest.c
	$(CC) -c -o $@ $(CFLAGS) $<

libtest.so: CFLAGS += -fPIC
libtest.so: libtest.o
	$(CC) $(LDFLAGS) $^ $(LFLAGS) -o $@
