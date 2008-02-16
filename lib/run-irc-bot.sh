#!/bin/sh

here=$(cd $(dirname "$0"); pwd)
cd "$here"

(
cat <<EOF
(cd "lib")
(load "tiny.arc")
(load "irc-bot.arc")
(= bot* (thread (irc "arcbot")))
(prn "bot is running in background")
EOF
cat /dev/stdin
) | ../arc.sh
