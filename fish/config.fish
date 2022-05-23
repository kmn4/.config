set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_CACHE_HOME $HOME/.cache
set -gx XDG_STATE_HOME $HOME/.local/state
set -gx MANPATH (string split ':' (manpath 2> /dev/null))

test -e $XDG_CONFIG_HOME/fish/extra-config.fish && . $XDG_CONFIG_HOME/fish/extra-config.fish
