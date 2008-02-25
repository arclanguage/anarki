#!/bin/bash
 
if [ -L "$0" ]; then
  arc_dir=`readlink $0`
else
  arc_dir=$0
fi
arc_dir=`dirname $arc_dir`

mzscheme="/Users/seano/Desktop/MzScheme v372/bin/mzscheme"

if [ "$(type -p rlwrap)" ]; then
  rlwrap -C arc "$mzscheme" -m -d "$arc_dir/as.scm"
else
  "$mzscheme" -m -d "$arc_dir/as.scm"
fi
