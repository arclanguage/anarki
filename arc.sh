#!/bin/bash
# Put a symlink to this script somewhere in your path.

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
arc.sh [-n] [-h]

OPTIONS
    -n
        No rlwrap for line-editing (useful inside emacs)

    -h
        Print help and exit
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

$rl racket -f $arc_dir/as.scm
