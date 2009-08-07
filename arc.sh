#!/bin/bash

arc_dir=$0

if [ -L "$0" ]; then
  arc_dir=$(readlink "$0")
fi

arc_dir=$(dirname "$arc_dir")
cd $arc_dir

if [ "$(type -p rlwrap)" -a ! "$1" = "--no-rl" ]; then
  rl="rlwrap -C arc"
fi

case "$(mzscheme --version)" in
  *v4.*) # AFAICT, there is no equivalent of --mute-banner for mzscheme v4
         $rl mzscheme -i --no-init-file --load as.scm;;
  *) $rl mzscheme --no-init-file --mute-banner --load as.scm;;
esac
