#
# Makefile to make mazegaki dictionary
#
SHELL = /bin/sh

DISTDATA = pd_kihon.yom greece.maz jukujiku.maz \
            t225.dat t300.dat t400.dat t450.dat t575.dat t675.dat \
            t900.dat t1200.dat t1353.dat \
            itaiji.maz

pkgdata_DATA =  $(DISTDATA) mazegaki.dic mkcertain.pl

EXTRA_DIST = mkcertain.pl step1.pl step2.pl tcode.u mk-mzdic \
             $(DISTDATA)

DICS = greece.maz jukujiku.maz itaiji.maz
RM = /bin/rm

mazegaki.dic: $(DICS)
	$(RM) -f $@
	$(EMACS) -q -no-site-file -batch -l $(srcdir)/mk-mzdic

# pd_kihon.yom: step1
# 	./step2.pl
# 	sort < pd_kihon.yom | uniq > tmpfile; mv tmpfile pd_kihon.yom
# 	sort < jukujiku.maz | uniq > tmpfile; mv tmpfile jukujiku.maz
# step1: tcode.u tankan.u kihon.u
# 	./step1.pl

clean:
	$(RM) -f mazegaki.dic
