#!/bin/bash -x

target=$HOME/.config
mkdir -p $target

git submodule update --init --recursive

thisdir=$(readlink -f $(dirname $0))
items=("bat" "emacs" "fish" "git" "latexmk")

for item in "${items[@]}"; do
    ln -sivT $thisdir/${item} $target/${item}
done
