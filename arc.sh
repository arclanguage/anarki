#!/bin/bash
 
arc_dir=$0

if [ -L "$0" ]; then
  arc_dir=$(readlink "$0")
fi

arc_dir=$(dirname "$arc_dir")

if [ "$(type -p rlwrap)" -a ! "$1" = "--no-rl" -a -z "${EMACS}" ]; then
  rl="rlwrap -C arc"
fi

if [ "$1" = "--no-rl" ]; then
  argv="${@:2}"
else
  argv="$@"
fi

$rl mzscheme --no-init-file --mute-banner --load-cd  "$arc_dir/as.scm" -- $argv
