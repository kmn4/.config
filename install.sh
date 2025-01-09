#!/bin/bash

set -euo pipefail

export VERSION_CONTROL=numbered

target="${XDG_CONFIG_HOME:-"$HOME/.config"}"
thisdir="$(readlink -f "$(dirname "$0")")"
. "$thisdir/install.conf" || exit 1

mkdir -p "$target"

symlink() {
    local realpath="$1"
    local linkpath="$2"
    [ "$(readlink -f "$linkpath")" = "$realpath" ] || ln -sbvT "$realpath" "$linkpath"
}

for item in "${items[@]}"; do
    symlink "$thisdir/$item" "$target/$item"
done

for item in $(ls -A "$thisdir/home"); do
    symlink "$thisdir/home/$item" "$HOME/$item"
done

# other files
[ -d ~/.gnupg ] || mkdir ~/.gnupg
[ -f ~/.gnupg/gpg-agent.conf ] || touch ~/.gnupg/gpg-agent.conf
grep '^pinentry-program' ~/.gnupg/gpg-agent.conf >/dev/null 2>&1 || \
echo 'pinentry-program /mnt/c/Program Files (x86)/Gpg4win/bin/pinentry.exe' >> ~/.gnupg/gpg-agent.conf
