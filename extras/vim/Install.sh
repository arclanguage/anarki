#!/bin/bash
install_to="$HOME/.vim"
if [ -d "$install_to/bundle" ]; then
  # See https://github.com/tpope/vim-pathogen
  install_to="$install_to/bundle/arc"
fi
for I in ftdetect ftplugin indent syntax ; do
    target="$install_to/$I"
    mkdir -p "$target"
    cp -f "$I/arc.vim" "$target/";
done
