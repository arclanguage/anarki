#!/bin/bash

if [ -L "$0" ]; then
  arc_dir=`readlink $0`
else
  arc_dir=$0
fi

ARC_DIR=`dirname $arc_dir`

if which rlwrap &> /dev/null; then
  rlwrap -C arc mzscheme -m -d "$ARC_DIR/as.scm"
else
  mzscheme -m -d "$ARC_DIR/as.scm"
fi
