
SUBDIRS = src

all:    Makefile
	@for d in $(SUBDIRS); do         \
	   if test ! -d $$d ; then        \
	       echo "=== Skipping subdir $$d" ; \
	   else                   \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ;                        \
	done

clean: Makefile
	rm -f *~
	@for d in $(SUBDIRS); do        \
	   if test ! -d $$d ; then        \
	       echo "=== Skipping subdir $$d" ; \
	   else                        \
	      (cd $$d && $(MAKE) $@) ; \
	   fi ;                        \
	done

install: all
	bin/eflex --install