CMD = erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell

NAME = gen_cnode
VERSION = 0.0.1
LIB_DIR = $(shell $(CMD))
INSTALL_DIR = $(LIB_DIR)/$(NAME)-$(VERSION)

ERL_FILES := $(wildcard *.erl)
BEAM_TARGETS := $(addsuffix .beam, $(basename $(ERL_FILES)))
APP_TARGETS := $(wildcard *.app)

default: all

all: $(BEAM_TARGETS) ;

%.beam: %.erl
	erlc -W $<

clean: 
	rm -f $(BEAM_TARGETS)
	rm -f ./*.*~

clobber: clean ;

install: all
	install -m 0755 -d $(INSTALL_DIR)
	install -m 0755 -d $(INSTALL_DIR)/ebin
	install -m 0755 -d $(INSTALL_DIR)/esrc
	install -m 0755 $(ERL_FILES) $(INSTALL_DIR)/esrc
	install -m 0755 $(BEAM_TARGETS) $(INSTALL_DIR)/ebin

uninstall: ;
