#!/bin/bash
# With args, acts as a wrapper for scripting in the arc language.
# Put a symlink to it somewhere in your path.
# Without args, starts up a repl.

function usage {
    echo "
arc [-n] [-h] [<file> [<file_args>]]

OPTIONS
    -n
        No rlwrap

    -h
        Print help and exit

    <file> [<file_args>]
        Don't start up a REPL; instead, execute the file, passing to it any file_args. When the file finishes executing, exit Arc.

EXAMPLES
    Start the Arc REPL.
        arc
    Start the Arc REPL without rlwrap
        arc -n
    Run the file \"file-to-run.arc\", passing to it the argument 3
        arc file-to-run.arc 3
"
    exit 1
}

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

RLWRAP=true
DO_HELP=false
while getopts nh opt; do
    case $opt in
        n)
            RLWRAP=false
            ;;
        h)
            DO_HELP=true
            ;;
        \? )
            exit 1
            ;;
    esac
done

#remove options from the arguments
shift $((OPTIND - 1))

if [[ $DO_HELP == true ]] ; then
    usage
fi

if [[ $RLWRAP == "true" ]] ; then
    rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
else
    rl=''
fi

if [ $# -gt 0 ]; then
    # there's a file given, execute it
    exec racket -f $arc_dir/boot.scm -e '(aload (vector-ref (current-command-line-arguments) 0))' "$@"
else
    #no file, start the REPL
    $rl racket -f $arc_dir/boot.scm -e '(tl)'
fi
