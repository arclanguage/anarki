#!/bin/sh

here=$(cd $(dirname "$0"); pwd)
cd "$here"

(
echo "(cd \"lib\")"
echo "(load \"irc-bot.arc\")" 
echo "(= bot* (thread (fn () (irc \"arcbot\"))))"
echo "(prn \"bot is running in background\")"
cat /dev/stdin
) | ../arc.sh
