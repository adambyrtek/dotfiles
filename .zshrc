# ~/.zshrc
# Sourced for interactive shells

# Package management for Debian
if which aptitude > /dev/null; then 
    alias a=aptitude
    alias sa="sudo aptitude"
elif which apt-get > /dev/null; then
    alias a=apt-get
    alias sa="sudo apt-get"
fi

# Package management for Gentoo
if which emerge > /dev/null; then
    alias em=emerge
    alias sem="sudo emerge"
    alias es="esearch -c"
    alias eq=equery
fi

# Aliases
alias !=history
alias '...'='../..'
alias '....'='../../..'
alias '.....'='../../../../'
alias l=less
alias g=grep
alias s=screen
alias psa="ps aux"
alias psgrep="ps aux | grep"
vimgrep() { vim -c "vimgrep /$1/ $*[2,-1]" -c copen }
si() { sudo /etc/init.d/$1 $2 }

# Global aliases
alias -g '***'='**/*'

# Default parameters
alias dirs="dirs -v"
alias history="history -iD"
alias du="du -chs"
alias df="df -h"
alias pstree="pstree -hG"
alias diff="diff -uN"
alias tree="tree -C"

# Simulate open command from Mac OS X
if which xdg-open > /dev/null; then
    alias open="xdg-open"
fi

# Colorized ls
which dircolors > /dev/null && eval `dircolors -b`
if [[ -z $MACOSX ]]; then
    alias ls="ls -hF --color=auto"
else
    # BSD has its own way
    alias ls="ls -hGF"
fi
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -lA"

# Load color module
autoload colors && colors

# Main prompt, empty line for readability
PROMPT='
%{${fg[yellow]}%}%n@%m:%~%#%{${reset_color}%} '

# Git branch right prompt
git_current_branch() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "[${ref#refs/heads/}]"
}
RPROMPT='%{${fg[red]}%}$(git_current_branch)%{${reset_color}%}'

# Spelling correction prompt
SPROMPT="%{${bg[red]}%}zsh: correct '%R' to '%r' [nyae]?%{${reset_color}%} "

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Slash not a part of a word
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# Watch users
#LOGCHECK=10
#watch=(all)

# Check mail
#MAILCHECK=10
#mailpath=(/home/alpha/mail/mbox/)

# Set Emacs style editing
bindkey -e

# Bind Ctrl-Left/Right
bindkey '\e[5D' backward-word
bindkey '\e[5C' forward-word

# Enable completion
autoload -U compinit && compinit

# Colors in completion
if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi 

# Grouping for completion types
zstyle ':completion:*:descriptions' format "%{${fg[magenta]}%}-- %d --%{$reset_color%}"
zstyle ':completion:*' group-name ""

# Split manual pages by sections
zstyle ':completion:*:manuals' separate-sections 'yes'

# Ignore internal zsh functions
zstyle ':completion:*:functions' ignored-patterns '_*'

# Describe all command options
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d*' 

# Process completion shows all processes, has menu and colors
zstyle ':completion:*:*:*:*:processes' command  'ps -a -u $USER -o pid,user,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31' 
zstyle ':completion:*:*:*:*:processes' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always

# Append to history file instantly
setopt incappendhistory

# Ignore entries starting with space in history
setopt histignorespace

# Ignore duplicates in history
setopt histignoredups

# Save timestamps in history file
setopt extendedhistory

# Safe redirections
setopt noclobber

# Don't show completion menu
setopt noautomenu

# Show list of completions
setopt autolist

# Enter directories without cd
setopt autocd

# Add every directory to the stack
setopt autopushd

# Spelling correction
setopt correct

# Don't beep when showing list of completions
setopt nolistbeep

# Terminal beep
#setopt nobeep

# Dynamic variable substitution in prompt
setopt prompt_subst

# Set screen or xterm title
title() {
    # Truncate long command and join lines
    t=$(print -Pn "%40>...>$1" | tr -d "\n")

    case $TERM in
        screen)
        print -Pn "\ek$t\e\\"
        ;;
        xterm*|rxvt)
        print -Pn "\e]0;$t\a"
        ;;
    esac
}

# Change title before and after each command
precmd() { title "zsh %~" }
preexec() { title "$1" }

# ls on each directory change
chpwd() { ls }

# man pages displayed in vim
man() { /usr/bin/man $* | col -b | vim -R -c 'set ft=man nomod nolist' -; }
