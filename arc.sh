#!/bin/bash
ARC_DIR=~/src/arc

if which rlwrap &> /dev/null; then
    rlwrap -C arc mzscheme -m -d "$ARC_DIR/as.scm"
else
    mzscheme -m -d "$ARC_DIR/as.scm"
fi
