# $Id$

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifndef mlisp
ifeq ($(on_windows),yes)
acldir = /c/Program Files/acl70
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

src_files = renpics.txt ChangeLog *.cl exif-utils/*.cl makefile

bin_dir = renpics-$(version)
src_dir = renpics-$(version)-src

bin_zip = DIST/$(bin_dir)-windows.zip
src_zip = DIST/$(src_dir).zip
readme  = DIST/renpics-$(version).txt

src-dist: FORCE
	rm -fr $(src_dir) $(src_zip)
	mkdir $(src_dir)
	tar cf - $(src_files) | (cd $(src_dir); tar xf -)
	find $(src_dir) -type f -print | zip -q $(src_zip) -@9
	rm -fr $(src_dir)

dist:	FORCE
	rm -fr $(bin_dir) $(bin_zip) $(readme)
	cp -rp renpics $(bin_dir)
	cp -p renpics.txt $(bin_dir)
	cp -p renpics.txt $(readme)
	find $(bin_dir) -type f -print | zip -q $(bin_zip) -@9
	rm -fr $(bin_dir)

clean: FORCE
	rm -fr *.fasl */*.fasl renpics testout *.gz *.bz2

FORCE:
