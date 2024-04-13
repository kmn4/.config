#!/bin/bash

target="$XDG_CONFIG_HOME"
thisdir=$(readlink -f $(dirname $0))
items=("bat" "emacs" "fish" "git" "latexmk")

mkdir -p $target

cd $thisdir

git submodule update --init --recursive

for item in "${items[@]}"; do
    [ "$(readlink -f "$target/$item")" = "$thisdir/$item" ] || ln -sivT "$thisdir/$item" "$target/$item"
done

declare -A files
while IFS= read -r -d '' file; do
    files["$(basename "$file")"]="$file"
done < <(find "$thisdir/home" -maxdepth 1 -type f -print0)
for item in "${!files[@]}"; do
    file="${files["$item"]}"
    [ "$(readlink -f "$HOME/$item")" = "$file" ] || ln -sivT "$file" "$HOME/$item"
done

#which fish >/dev/null && fish -c 'curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher && fisher update'

