#!/bin/bash
# With args, acts as a wrapper for scripting in the arc language.
# Put a symlink to it somewhere in your path.
# Without args, starts up a repl.

# argument parsing
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
            # error; show usage anyway
            DO_HELP=true
            ;;
    esac
done

# remove options from the arguments
shift $((OPTIND - 1))

if [[ $DO_HELP == true ]] ; then
    echo "
arc [-n] [-h] [<file> [<file_args>]]

OPTIONS
    -n
        No rlwrap for line-editing

    -h
        Print help and exit

    <file> [<file_args>]
        Don't start up a REPL; instead, execute the file, passing to it any file_args. When the file finishes executing, exit Arc.

EXAMPLES
    Start the Arc REPL:
        arc
    Start the Arc REPL without any line-editing smarts:
        arc -n
    Run the file \"file-to-run.arc\", passing to it the argument 3
        arc file-to-run.arc 3
"
    exit 1
fi

if [ $(uname) = "Darwin" ]; then
  if which greadlink >&/dev/null; then
    if which racket >&/dev/null; then
        arc_dir=$(dirname "$(greadlink -f "$0")")
    else
        echo 'Please do "brew install racket && raco pkg install --auto drracket"'
        exit 1
    fi
  else
    echo 'Please do "brew install coreutils".'
    exit 1
  fi
else
  arc_dir=$(dirname "$(readlink -f "$0")")
fi

# useful error if rlwrap is enabled but not yet installed
if $RLWRAP && [ ! `which rlwrap 2>/dev/null` ]
then
  if [ `uname` = "Darwin" ]
    then echo "Please run: \"brew install rlwrap\""
    else echo "Please install rlwrap"
  fi
  echo "or run arc without rlwrap: \"./arc -n\""
  exit 1
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
