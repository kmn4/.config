set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx XDG_STATE_HOME $HOME/.local/state
set -gx MANPATH (string split ':' (manpath 2> /dev/null))
set -gx LANG ja_JP.UTF-8

not status is-login || set -gx GPG_TTY (tty)
status is-login && which hugo > /dev/null && eval (hugo completion fish)

if not set -q WIN_HOME && is-wsl2
    set -U WIN_HOME (wslpath (wslvar USERPROFILE))
end

alias ssh="ssh.exe"
alias ssh-add="ssh-add.exe"

test -e $XDG_CONFIG_HOME/fish/extra-config.fish && . $XDG_CONFIG_HOME/fish/extra-config.fish
