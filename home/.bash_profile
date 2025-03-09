# shellcheck shell=bash
# shellcheck disable=SC2155
eval "$(ssh-agent -s)" > /dev/null 2>&1
trap 'kill $SSH_AGENT_PID' EXIT
export LANG=ja_JP.UTF-8
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"
export MANPATH="$(manpath 2> /dev/null)"

export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"
command -v hugo &> /dev/null  && eval "$(hugo completion bash)"
test -d "$XDG_DATA_HOME/coursier/bin" && export PATH="$XDG_DATA_HOME/coursier/bin:$PATH"
test -d "$HOME/.local/bin" && export PATH="$HOME/.local/bin:$PATH"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
    if test -f "$HOME/.bash_profile.local"
    then
        . "$HOME/.bash_profile.local"
    fi
fi

# history
export HISTSIZE=
export HISTFILESIZE=
export HISTFILE=~/.shell_history
export HISTTIMEFORMAT="[%FT%T%z] "
