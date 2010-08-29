SHELL = /bin/sh
INSTALL_SCRIPT = @INSTALL_SCRIPT@

bin_SCRIPTS = tcinput

pkgdata_DATA = init.el skk-startup.el tc-skki.el load-path.el
EXTRA_DIST = init.el skk-startup.el tc-skki.el

load-path.el:
	echo "(setq load-path (nconc load-path (cons \"@lispdir@\" nil)))" \
              > load-path.el
