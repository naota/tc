#
# Makefile for Lisp sources of tc
#
SHELL = /bin/sh

PREFILE = tc-pre.el

COMMON_EL_FILES = tc-setup.el tc.el tc-help.el tc-complete.el tc-jiscode.el \
	tc-bushu.el tc-mazegaki.el tc-util.el tc-tbl.el tutc-tbl.el eelll.el \
	tc-inst.el tc-stat.el tc-ja-alnum.el

lisp_LISP = tc-sysdep.el $(COMMON_EL_FILES) tc-is18.el tc-is19.el tc-is20.el \
	 tc-bitmap.el tc-image.el tc-mkmzdic.el $(PREFILE)
ELCFILES = compile.log
noinst_LISP = make-elc guess tc-pre-base

EXTRA_DIST = $(lisp_LISP) $(noinst_LISP) add-site-info

$(PREFILE): tc-pre-base
	$(SHELL) add-site-info tc-pre-base $(PREFILE) \
		"@datadir@/@PACKAGE@/"

compile.log: $(PREFILE)
	$(SHELL) add-site-info tc-pre-base $(PREFILE) \
		"@datadir@/@PACKAGE@/"
	$(EMACS) -q -no-site-file -batch -l $(srcdir)/make-elc \
		$(COMMON_EL_FILES) $(EL_FILES) 2> compile.log

clean-generic:
	rm -f *.elc $(PREFILE)
