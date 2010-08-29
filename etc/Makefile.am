SHELL = /bin/sh

DISTDATA = EELLLTXT EELLLTXT.tut bushu.rev symbol.rev tc-bitmap-8x16

pkgdata_DATA = $(DISTDATA) bushu.expand bushu.index2
EXTRA_DIST = $(DISTDATA) mk-bdic

bushu.expand bushu.index2: bushu.rev
	@echo '*** compiling bushu dictionary ***'
	@echo '*** this may take a few minutes ***'
	$(EMACS) -q -no-site-file -batch -l $(srcdir)/mk-bdic

clean:
	$(RM) bushu.expand bushu.index2
