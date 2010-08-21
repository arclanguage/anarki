#!/bin/sh

arc_dir=$(dirname "$(readlink "$0")")

if [ "$1" = "--no-rl" ]; then
    shift
elif [ "$(type rlwrap)" ]; then
    rl="rlwrap -C arc"
fi

# I wish there were some cleaner way to do this
case "$(mzscheme --version)" in
    *v4.*) plt4=yes;;
    *v5.*) plt4=yes;; # compatible with v4 for the use in this script
    *) plt4=no;;
esac

if [ $# -gt 0 ]
then repl='#f'
else
    # It seems like there ought to be a program for determining whether standard
    # input is a tty, but I can't find one; so we use mzscheme instead.
    if [ $plt4 = yes ]
    then repl=$(mzscheme -e '(terminal-port? (current-input-port))')
    else repl=$(mzscheme -mve '(display (terminal-port? (current-input-port)))')
    fi
fi

opts="--no-init-file"

if [ $plt4 = yes ]; then
    if [ $repl = '#t' ]
    then
        # AFAICT, there is no equivalent of --mute-banner for mzscheme v4.
        # Please do NOT add -r/--script here, this removes the ability to access
        # the Scheme REPL. See http://arclanguage.org/item?id=10356.
        opts="$opts --repl --load"
    else opts="$opts --script"
    fi
else
    if [ $repl = '#t' ]
    then opts="$opts --mute-banner --load"
    else opts="$opts --script"
    fi
fi

$rl mzscheme $opts $arc_dir/as.scm $@
