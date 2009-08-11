#!/bin/bash

arc_dir=$0

if [ -L "$0" ]; then
  arc_dir=$(readlink "$0")
fi

arc_dir=$(dirname "$arc_dir")

if [ "$(type -p rlwrap)" -a ! "$1" = "--no-rl" ]; then
  rl="rlwrap -C arc"
fi

# I wish there were some cleaner way to do this
case "$(mzscheme --version)" in
    *v4.*) plt4=yes;;
    *) plt4=no;;
esac

opts="--no-init-file"

# It seems like there ought to be a program for determining whether standard
# input is a tty, but I can't find one; so we use mzscheme instead.
if [ $plt4 = yes ]; then
    case $(mzscheme -e '(terminal-port? (current-input-port))') in
        '#t') # AFAICT, there is no equivalent of --mute-banner for mzscheme v4
              opts+=" --repl --load";;
        *) opts+=" --script";;
    esac
else
    case $(mzscheme -mve '(display (terminal-port? (current-input-port)))') in
        '#t') opts+=" --mute-banner --load";;
        *) opts+=" --script";;
    esac
fi

$rl mzscheme $opts $arc_dir/as.scm $@
