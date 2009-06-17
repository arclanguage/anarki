#!/bin/bash
for I in ftplugin indent syntax ; do
    target="$HOME/.vim/$I"
    mkdir -p "$target"
    cp -f "$I/arc.vim" "$target/";
done
