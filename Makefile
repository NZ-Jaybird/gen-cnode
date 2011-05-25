include ./Makedefs

SUBDIRS := lib c_src esrc tests

.PHONY: subdirs $(SUBDIRS)

all clean clobber: $(SUBDIRS) ;

makedir:
	install --mode=755 -d $(INSTALL_DIR)
	install --mode=755 -d $(SRC_DIR)
	install --mode=755 -d $(PRIV_DIR)
	install --mode=755 -d $(EBIN_DIR)

install: makedir $(SUBDIRS) 

uninstall: 
	rm -rf $(INSTALL_DIR)

$(SUBDIRS):
	$(warning $(MAKECMDGOALS))
	$(MAKE) -C $@ $(MAKECMDGOALS)
