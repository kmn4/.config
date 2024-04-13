# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

export GPG_TTY="$(tty)"
setxkbmap jp
export LANG=ja_JP.UTF-8
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"
export MANPATH="$(manpath 2> /dev/null)"
export WIN_HOME="$(wslpath $(wslvar USERPROFILE))"

which hugo > /dev/null  && eval "$(hugo completion bash)"

# history
export HISTSIZE=
export HISTFILESIZE=
export HISTFILE=~/.shell_history
export HISTTIMEFORMAT="[%FT%T%z] "
