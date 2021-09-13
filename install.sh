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

thisdir=$(readlink -f $(dirname $0))

for item in "${from_config_dir[@]}"; do
    ln -s $thisdir/${item} $HOME/.config/${item}
done

for item in "${from_home_dir[@]}"; do
    ln -s $thisdir/${item} $HOME/${item}
done
