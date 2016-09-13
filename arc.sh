#!/bin/bash
# Put a symlink to this script somewhere in your path.

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

        if ! which rlwrap >&/dev/null
        then
          echo 'Please run "brew install rlwrap"'
          echo 'Running without rlwrap; some keys on your keyboard may not work as expected'
        else
          rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
        fi
        ;;

    *)
        if ! which racket >&/dev/null
        then
          echo "Please install racket with your OS's package manager (apt-get, dpkg, rpm, yum, pacman, etc.)"
          echo "Or go to http://racket-lang.org for more detailed instructions."
          exit 1
        fi
        arc_dir=$(dirname "$(readlink -f "$0")")

        if ! which rlwrap >&/dev/null
        then
          echo "Please install rlwrap with your OS's package manager (apt-get, dpkg, rpm, yum, pacman, etc.)"
          echo 'Running without rlwrap; some keys on your keyboard may not work as expected'
        else
          rl='rlwrap --complete-filenames --quote-character "\"" --remember --break-chars "[]()!:~\"" -C arc'
        fi
esac

$rl racket -f $arc_dir/as.scm
