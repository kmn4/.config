# XDG
set -x XDG_DATA_HOME $HOME/.local/share
set -x XDG_CONFIG_HOME $HOME/.config
set -x XDG_CACHE_HOME $HOME/.cache
set -x SPACEMACSDIR $XDG_CONFIG_HOME/spacemacs
set -x JDK_HOME /usr/lib/jvm/java-11-openjdk-amd64
set -l BREWHOME /home/linuxbrew/.linuxbrew
set -gxp INFOPATH $BREWHOME/info
set -p INFOPATH /usr/local/texlive/2021/texmf-dist/doc/info
set -x GOPATH $HOME/go
set -gx MANPATH (string split ':' (manpath 2> /dev/null))
set -x BIBINPUTS $HOME/Dropbox/lab/bib
if not contains $BREWHOME/bin $PATH
    set -xa PATH $BREWHOME/bin
end

# Emacs vterm
function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end
