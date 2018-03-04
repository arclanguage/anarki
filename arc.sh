#!/usr/bin/env bash
# Put a symlink to this script somewhere in your path.
#
# To run an Arc program, pass it in followed by any args to it.
# To open an interactive prompt (REPL), run without args.
# To open a REPL after running a program, pass in the '-i' flag.

# argument parsing
RLWRAP=true
DO_HELP=false
REPL=maybe
while getopts nih opt; do
    case $opt in
        n)
            RLWRAP=false
            ;;
        i)
            REPL=definitely
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
arc.sh [-n] [-i] [-h] [<file> [<file_args>]]

OPTIONS
    -n
        No rlwrap for line-editing (useful inside emacs)

    -i
        Start an interactive session (REPL)

    -h
        Print help and exit

    <file> [<file_args>]
        Run the given file, passing to it any file_args. When the file finishes running, start an interactive session (REPL) if '-i' was explicitly provided.

EXAMPLES
    Start the Arc REPL:
        arc.sh
    Start the Arc REPL without any line-editing smarts:
        arc.sh -n
    Run the file \"x.arc\", passing to it the argument '3':
        arc.sh x.arc 3
    Run the file \"x.arc\", passing to it the argument '3' -- and then start the Arc REPL:
        arc.sh -i x.arc 3
    Run the file \"x.arc\", passing to it the arguments '-i' and '3':
        arc.sh x.arc -i 3
"
    exit 1
fi

# try to suggest solutions to the most common issues people run into
case $(uname) in
    "Darwin")
        if ! which greadlink >&/dev/null
        then
          echo 'Please do "brew install coreutils."'
          exit 1
        fi
        if ! which racket >&/dev/null
        then
          echo 'Please run "brew install racket && raco pkg install --auto drracket"'
          exit 1
        fi
        arc_dir=$(dirname "$(greadlink -f "$0")")
        if [[ $RLWRAP == true ]] && ! which rlwrap >&/dev/null
        then
          echo 'Please run "brew install rlwrap"'
          echo 'Or run arc without rlwrap: "./arc.sh -n"'
          exit 1
        fi
        ;;

    "Linux")
        if ! which racket >&/dev/null
        then
          echo "Please install racket with your OS's package manager (apt-get, dpkg, rpm, yum, pacman, etc.)"
          echo "Or go to http://racket-lang.org for more detailed instructions."
          exit 1
        fi
        arc_dir=$(dirname "$(readlink -f "$0")")
        if [[ $RLWRAP == true ]] && ! which rlwrap >&/dev/null
        then
          echo "Please install rlwrap with your OS's package manager (apt-get, dpkg, rpm, yum, pacman, etc.)"
          echo 'Or run arc without rlwrap: "./arc.sh -n"'
          exit 1
        fi
        ;;

    *)
	arc_dir=$(pwd)
esac

if [ $# -gt 0 ]; then
  # there's a file given, execute it
  load="(aload-with-main-settings (vector-ref (current-command-line-arguments) 0))"
fi

if [[ $REPL == definitely || ( $REPL == maybe && $# -eq 0 ) ]]; then
  # start an interactive prompt
  if [[ $RLWRAP == true ]]; then
    rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
  fi
  repl="(tl-with-main-settings)"
fi

$rl racket -t "$arc_dir/boot.rkt" -e "(anarki-init-in-main-namespace-verbose) $load $repl" "$@"
