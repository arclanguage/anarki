#!/bin/bash
 
arc_dir=$0

if [ -L "$0" ]; then
  arc_dir=$(readlink "$0")
fi

arc_dir=$(dirname "$arc_dir")

if [ "$(type -p rlwrap)" -a ! "$1" = "--no-rl" ]; then
  rl="rlwrap -C arc"
fi

$rl mzscheme --no-init-file --mute-banner --load-cd  "$arc_dir/as.scm" -- "$@"
