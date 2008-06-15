#!/bin/bash

arc_dir=$(dirname $(readlink --canonicalize "$0"))

if [ "$1" == "--no-rl" ]; then
    shift
else
    # If there's a program named "rlwrap", and it would actually be
    # useful, then use it.
    if [ "$(type -p rlwrap)" -a -z "${EMACS}" ]; then
        rl="rlwrap -C arc"
    fi
fi

$rl mzscheme --no-init-file --mute-banner --load-cd  "$arc_dir/as.scm" -- "$@"
