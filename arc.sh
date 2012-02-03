#!/bin/sh

arc_dir=$(dirname "$(readlink "$0")")

if [ "$1" = "--no-rl" ]; then
    shift
elif [ "$(type rlwrap)" ]; then
    rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
fi

$rl racket -f $arc_dir/as.scm $@
