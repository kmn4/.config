#!/bin/bash

set -euo pipefail

export VERSION_CONTROL=numbered

target="${XDG_CONFIG_HOME:-"$HOME/.config"}"
thisdir="$(readlink -f "$(dirname "$0")")"
. "$thisdir/install.conf" || exit 1

mkdir -p "$target"

for item in "${items[@]}"; do
    [ "$(readlink -f "$target/$item")" = "$thisdir/$item" ] || ln -sbvT "$thisdir/$item" "$target/$item"
done

declare -A files
while IFS= read -r -d '' file; do
    files["$(basename "$file")"]="$file"
done < <(find "$thisdir/home" -maxdepth 1 -type f -print0)
for item in "${!files[@]}"; do
    file="${files["$item"]}"
    [ "$(readlink -f "$HOME/$item")" = "$file" ] || ln -sbvT "$file" "$HOME/$item"
done

# other files
[ -d ~/.gnupg ] || mkdir ~/.gnupg
[ -f ~/.gnupg/gpg-agent.conf ] || touch ~/.gnupg/gpg-agent.conf
grep '^pinentry-program' ~/.gnupg/gpg-agent.conf >/dev/null 2>&1 || \
echo 'pinentry-program /mnt/c/Program Files (x86)/Gpg4win/bin/pinentry.exe' >> ~/.gnupg/gpg-agent.conf
