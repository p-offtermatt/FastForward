.PHONY : _cppcheck _style doxygen

DISTCLEANFILES =

# Texinfo

info_TEXINFOS = lola.texi
lola_TEXINFOS = grammars outputs

DISTCLEANFILES += lola.aux lola.cp lola.cps lola.fn lola.ky lola.log lola.pg lola.toc lola.tp lola.vr

# Manpage

man1_MANS = lola.man

# add PDF and Manpage to distribution

EXTRA_DIST = lola.man lola.pdf

lola.man:
	$(AM_V_GEN)$(HELP2MAN) $(top_builddir)/src/lola --help-option=--detailed-help --output=$@

# Rest

doxygen:
	doxygen

_style:
	for FILE in `find ../src -type f -name "*.[ch]" -o -name "*.cc" -o -name "*.inc"`; do astyle --options=astylerc $$FILE; done

_cppcheck:
	cppcheck --enable=all --inconclusive --xml --xml-version=2 ../src --force 2> cppcheck.xml

_cpplint:
	-for FILE in `find ../src -type f -name "*.[ch]" -o -name "*.cc" -o -name "*.inc"`; do ./cpplint.py --filter=-whitespace/braces,-legal/copyright,-build/header_guard,-whitespace/line_length,-whitespace/labels,-whitespace/newline $$FILE 2>&1; done

#############################################################################
# run the "gendocs.sh" script to create a manual in several formats
#############################################################################
manual: $(info_TEXINFOS)
	./gendocs.sh $(PACKAGE) "$(PACKAGE_NAME)"
