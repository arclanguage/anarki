#!/bin/bash
# With args, acts as a wrapper for scripting in the arc language.
# Put a symlink to it somewhere in your path.
# Without args, starts up a repl.

if [ $(uname) = "Darwin" ]; then
  if which -s greadlink; then
    arc_dir=$(dirname "$(greadlink -f "$0")")
  else
    echo 'Please do "brew install coreutils".'
    exit 1
  fi
else
  arc_dir=$(dirname "$(readlink -f "$0")")
fi

# Args; batch mode. Load first arg and pass remaining args to it.
if [ $# -gt 0 ]
then
  exec racket -f $arc_dir/boot.scm -e '(aload (vector-ref (current-command-line-arguments) 0))' "$@"
  # then exits
fi

# No args; interactive mode
if [ "$(type rlwrap)" ]; then
  rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
fi

$rl racket -f $arc_dir/boot.scm -e '(tl)'
