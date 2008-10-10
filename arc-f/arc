#!/bin/bash

if [ `uname` = 'Darwin' ] ; then
    this_script=$(readlink "$0")
    this_script=${this_script:="$0"}
    arc_dir=$(dirname "$this_script")
else
    arc_dir=$(dirname $(readlink --canonicalize "$0"))
fi

if [ "$1" == "--no-rl" ]; then
    shift
else
    # If there's a program named "rlwrap", and it would actually be
    # useful, then use it.
    if [ "$(type -p rlwrap)" -a -z "${EMACS}" ]; then
        rl="rlwrap -C arc"
    fi
fi

export arc_dir

$rl mzscheme --no-init-file --mute-banner\
	--load "$arc_dir/as.scm" \
	-- "$@"
