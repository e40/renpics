# $Id$

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifndef mlisp
ifeq ($(on_windows),yes)
acldir = /c/Program Files/acl62
mlisp = "$(acldir)/mlisp.exe" +B +cn
else
mlisp = mlisp
endif
endif

default:	build

all:	clean build install

build:	FORCE
	rm -fr renpics
	$(mlisp) -L buildit.cl -kill -batch

install: FORCE
ifeq ($(on_windows),yes)
	cp -p renpics/*.* c:/bin
else
	rm -fr /usr/local/renpics
	mkdir /usr/local/renpics
	cp -p renpics/* /usr/local/renpics
	rm -f /usr/local/bin/renpics
	ln -s /usr/local/renpics/renpics /usr/local/bin/renpics
endif

version = \
 $(shell grep Revision: renpics.cl | sed 's/.*Revision: \([0-9.]*\).*/\1/')

src_files = readme.txt ChangeLog *.cl exif-utils/*.cl *.gif makefile

bin_dir = renpics-$(version)
src_dir = renpics-$(version)-src
bin_tar = $(bin_dir)-linux-glibc-2.1.tar

bin_gz = DIST/$(bin_tar).gz
bin_bz2 = DIST/$(bin_tar).bz2
src_gz = DIST/$(src_dir).tar.gz
readme = DIST/readme-$(version).txt

src-dist: FORCE
	rm -fr $(src_dir) $(src_gz)
	mkdir $(src_dir)
	tar cf - $(src_files) | (cd $(src_dir); tar xf -)
	tar zcf $(src_gz) $(src_dir)
	rm -fr $(src_dir)


dist:	FORCE
	rm -fr $(bin_dir) $(bin_tar) $(bin_gz) $(bin_bz2) $(readme)
	cp -rp renpics $(bin_dir)
	cp -p readme.txt $(bin_dir)
	cp -p readme.txt $(readme)
	tar cf $(bin_tar) $(bin_dir)
	gzip -c9 < $(bin_tar) > $(bin_gz)
	bzip2 -c9 < $(bin_tar) > $(bin_bz2)
	rm -f $(bin_tar)
	rm -fr $(bin_dir)

clean: FORCE
	rm -fr *.fasl */*.fasl renpics testout *.gz *.bz2

test: FORCE
	rm -fr testout
	renpics/renpics test/ testout/

FORCE:
