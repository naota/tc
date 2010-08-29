#
# Makefile for tc package
#
SHELL = /bin/sh

SUBDIRS = lisp doc etc kinput2 mazegaki bushu-util skkinput3

EXTRA_DIST = ChangeLog.old

uninstall:
	-rmdir $(pkgdatadir)

install-lisp:
	(cd $(srcdir)/lisp; $(MAKE) install)
