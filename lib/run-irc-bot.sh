#!/bin/sh

here=$(cd $(dirname "$0"); pwd)
cd "$here"

(
echo "(do ($ (current-directory \""$here"\"))"
echo " (load \"irc-client.arc\"))" 
echo "(irc \"arcbot\")"
) | ../arc.sh
