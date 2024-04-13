# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# https://gist.github.com/snaka/3837457
parse_git_dirty() {
    [ "$(git status 2> /dev/null | tail -n1)" != "nothing to commit, working tree clean" ] && echo '*'
}
parse_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ (\1$(parse_git_dirty))/"
}
__prompt_command() {
    local pipestatus="${PIPESTATUS[@]}"
    local exitcode="$(echo "$pipestatus" | tr " " "\n" | tail -1)"
    local pipecode="$(echo -n "$pipestatus" | tr ' ' '|')"
    [ "$exitcode" != 0 ] && local promptcode=" \[\e[31m\][$pipecode]\[\e[0m\]"
    PS1=
    PS1+='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]'
    PS1+='$(parse_git_branch)'
    PS1+="$promptcode"
    PS1+='$ '
}

if [ "$color_prompt" = yes ]; then
    PROMPT_COMMAND=__prompt_command
    PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

[ -e "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

[ -x /usr/bin/terraform ] && complete -C /usr/bin/terraform terraform

stty werase undef; bind '"\C-w": unix-filename-rubout'
bind 'set completion-ignore-case on'
bind 'set colored-stats on'
bind 'set colored-completion-prefix on'
bind 'set show-all-if-ambiguous on'
bind '"\ei": menu-complete'
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'

alias ssh="ssh.exe"
alias ssh-add="ssh-add.exe"
alias egui="emacsclient -cn -a ''"
alias ecui="emacsclient -nw -a ''"
alias firefox.exe="'/mnt/c/Program Files/Mozilla Firefox/firefox.exe'"
which firefox > /dev/null || alias firefox="firefox.exe"

alias update_apt='which apt > /dev/null && sudo apt update && sudo apt upgrade && sudo apt autoremove'
alias update_winget='which powershell.exe > /dev/null && powershell.exe -Command "winget upgrade"'
alias update_emacs_packages='which emacs > /dev/null && emacsclient -e "(package-upgrade-packages-with-logging)"'
alias update_cs='which cs > /dev/null && cs update'
alias update_brew='which brew > /dev/null && brew update && brew upgrade && brew autoremove'
alias update_npm='which npm > /dev/null && sudo npm update -g'
alias update_rustup='which rustup > /dev/null && rustup update'
alias update_ghcup='which ghcup > /dev/null && ghcup update'
alias update_stack='which stack > /dev/null && stack update && stack upgrade'

update_all() {
    update_apt
    update_cs
    update_emacs_packages
    update_brew
    update_npm
    update_rustup
    update_ghcup && update_stack
    update_winget
}
