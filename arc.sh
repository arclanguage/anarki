#!/bin/bash
 
if [ -L "$0" ]; then
  arc_dir=`readlink $0`
else
  arc_dir=$0
fi
arc_dir=`dirname $arc_dir`

mzscheme_dir=$arc_dir/../third-party/mzscheme
mzscheme=$mzscheme_dir/bin/mzscheme
if [ ! -e $mzscheme ]; then
  cd $mzscheme_dir
  ./configure && make && make install
fi

if [ -e `which rlwrap` ]; then
  rlwrap -C arc $mzscheme -m -d "$arc_dir/as.scm"
else
  $mzscheme -m -d "$arc_dir/as.scm"
fi
