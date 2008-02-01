#!/bin/sh

if [[ ! -e third-party/mzscheme/bin/mzscheme ]]; then
	root_dir=`pwd`
	cd third-party/mzscheme/src
	./configure && make && make install
    cd $root_dir
fi

cd core

if [ -e `which rlwrap` ] &> /dev/null; then 
	rlwrap -C arc ../third-party/mzscheme/bin/mzscheme -m -f as.scm	
else 
	../third-party/mzscheme/bin/mzscheme -m -f as.scm
fi;
