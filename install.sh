#!/bin/bash

# linked from ~
from_home_dir=(
    ".Xmodmap"
)
target=$HOME/.config



git submodule update --init --recursive
mkdir -p $target
thisdir=$(readlink -f $(dirname $0))

for item in $(find $thisdir -maxdepth 1 -mindepth 1); do
    mv -i ${item} $target
done

cd $target

for item in "${from_home_dir[@]}"; do
    ln -siv $(pwd)/${item} $HOME/${item}
done
