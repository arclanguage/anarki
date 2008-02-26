#!/bin/bash
 
if [ -L "$0" ]; then
  arc_dir=`readlink $0`
else
  arc_dir=$0
fi
arc_dir=`dirname $arc_dir`

mzscheme=mzscheme

if [ "$(type -p rlwrap)" -a ! "$1" = "--no-rl" ]; then
  rlwrap -C arc "$mzscheme" -m -d "$arc_dir/as.scm"
else
  "$mzscheme" -m -d "$arc_dir/as.scm"
fi
