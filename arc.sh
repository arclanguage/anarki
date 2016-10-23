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
arc.sh [-n] [-h] [<file> [<file_args>]]

OPTIONS
    -n
        No rlwrap for line-editing (useful inside emacs)

    -h
        Print help and exit

    <file> [<file_args>]
        Don't start up a REPL; instead, execute the file, passing to it any file_args. When the file finishes executing, exit Arc.

EXAMPLES
    Start the Arc REPL:
        arc.sh
    Start the Arc REPL without any line-editing smarts:
        arc.sh -n
    Run the file \"file-to-run.arc\", passing to it the argument 3
        arc.sh file-to-run.arc 3
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
        if $RLWRAP && ! which rlwrap >&/dev/null
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
        if $RLWRAP && ! which rlwrap >&/dev/null
        then
          echo "Please install rlwrap with your OS's package manager (apt-get, dpkg, rpm, yum, pacman, etc.)"
          echo 'Or run arc without rlwrap: "./arc.sh -n"'
          exit 1
        fi
        ;;

    *)
        echo "Sorry, this script doesn't run on your platform $(uname) yet. Please create a new issue at https://github.com/arclanguage/anarki/issues."
        exit 1
esac

if [[ $RLWRAP == "true" ]] ; then
    rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
else
    rl=''
fi

if [ $# -gt 0 ]; then
    # there's a file given, execute it
    exec racket -t "$arc_dir/boot.scm" -e '(aload (vector-ref (current-command-line-arguments) 0))' "$@"
else
    #no file, start the REPL
    $rl racket -t "$arc_dir/boot.scm" -e '(tl)'
fi
