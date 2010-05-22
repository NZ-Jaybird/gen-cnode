
SUBDIRS := lib c_src esrc tests

.PHONY: subdirs $(SUBDIRS)

all clean clobber install uninstall: $(SUBDIRS) ;

$(SUBDIRS):
	$(warning $(MAKECMDGOALS))
	$(MAKE) -C $@ $(MAKECMDGOALS)
