#!/bin/bash

git submodule update --init --recursive

# linked from ~/.config
from_config_dir=(
    "bat"
    "emacs"
    "fish"
    "git"
    "latexmk"
)

# linked from ~
from_home_dir=(
    ".Xmodmap"
)

mkdir -p $HOME/.config

for item in "${from_config_dir[@]}"; do
    ln -s $HOME/config/${item} $HOME/.config/${item}
done

for item in "${from_home_dir[@]}"; do
    ln -s $HOME/config/${item} $HOME/${item}
done
