# $Id$

mlisp ?= mlisp

default:	build

all:	clean build install

build:	FORCE
	rm -fr renpics
	$(mlisp) -L buildit.cl -kill -batch

install: FORCE
	rm -fr /usr/local/renpics.old
	-mv /usr/local/renpics /usr/local/renpics.old
	mkdir /usr/local/renpics
	cp -rp renpics/* /usr/local/renpics
	rm -f /usr/local/bin/renpics
	ln -s /usr/local/renpics/renpics /usr/local/bin/renpics

clean: FORCE
	rm -fr *.fasl */*.fasl renpics testout *.gz *.bz2

FORCE:
