SHELL = /bin/sh

pkgdata_DATA = manual.pdf

EXTRA_DIST = sample.tc tc.info $(pkgdata_DATA)

info_TEXINFOS = tc.texinfo
TEXINFO_TEX = no-texinfo.tex

MAKEINFO = $(EMACS) -q -no-site-file -batch -l texinfmt -f batch-texinfo-format

all: manual.pdf

manual.pdf: tc.texinfo
	ptex tc.texinfo
	$(EMACS) -q -no-site-file -batch -l $(srcdir)/texindex.el \
		-f batch-texindex tc.cp tc.cp tc.fn tc.ky tc.tp tc.vr
	ptex tc.texinfo
	mv tc.dvi manual.dvi
	dvipdfm manual.dvi
