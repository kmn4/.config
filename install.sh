#!/bin/bash -x

target=$HOME/.config
thisdir=$(readlink -f $(dirname $0))
items=("bat" "emacs" "fish" "git" "latexmk")

mkdir -p $target

cd $thisdir

git submodule update --init --recursive

for item in "${items[@]}"; do
    ln -sivT $thisdir/${item} $target/${item}
done

cd -
