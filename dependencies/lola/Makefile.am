# tell aclocal about the macros in the "m4" folder
ACLOCAL_AMFLAGS = -I m4 --install

SUBDIRS = src utils tests doc

EXTRA_DIST = examples

# required to make test cases pass in "make distcheck"
AM_DISTCHECK_CONFIGURE_FLAGS = --enable-debug

svn-clean: maintainer-clean
	rm -fr configure aclocal.m4 src/config.h.in doc/mdate-sh src/config-log.h build-aux
	for DIR in $(DIST_SUBDIRS) .; do rm -f $$DIR/Makefile.in; done

all:
	@echo ""
	@echo "============================================================"
	@echo "  Successfully compiled $(shell $(top_builddir)/src/lola --version | $(AWK) 'NR==1')."
	@echo "  -> check out LoLA's help with \`src/lola$(EXEEXT) --help'"
	@echo "  -> install LoLA to @prefix@/bin with \`make install'"
	@echo "============================================================"
	@echo ""
