# XDG
set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_CACHE_HOME $HOME/.cache
set -x SPACEMACSDIR $XDG_CONFIG_HOME/spacemacs
set -x JDK_HOME /usr/lib/jvm/java-11-openjdk-amd64
set -l BREWHOME /home/linuxbrew/.linuxbrew
set -x INFOPATH $BREWHOME/info $INFOPATH
set -x GOPATH $HOME/go
set -gx MANPATH (string split ':' (manpath 2> /dev/null))
set -x BIBINPUTS $HOME/Dropbox/lab/bib
if not contains $BREWHOME/bin $PATH
    set -xa PATH $BREWHOME/bin
end

# opam configuration
# source $HOME/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
# eval (opam env)
