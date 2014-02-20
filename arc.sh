#!/bin/sh


if [ $(uname) == "Darwin" ]; then
    if which -s greadlink; then
        arc_dir=$(dirname "$(greadlink -f "$0")")
    else
        echo 'Please do "brew install coreutils".'
        exit 1
    fi
else
    arc_dir=$(dirname "$(readlink -f "$0")")
fi

if [ "$1" = "--no-rl" ]; then
    shift
elif [ "$(type rlwrap)" ]; then
    rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
fi

$rl racket -f $arc_dir/as.scm $@
