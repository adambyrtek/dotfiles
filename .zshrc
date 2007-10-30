# ~/.zshrc
# Sourced for interactive shells

# Package management for Debian
if which aptitude > /dev/null; then 
   alias a=aptitude
   alias sa="sudo aptitude"
else if which apt-get > /dev/null
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

# Common aliases
alias !=history
alias dirs="dirs -v"
alias du="du -chs"
alias df="df -h"
alias pstree="pstree -hG"
alias diff='diff -uN'
si () { sudo /etc/init.d/$1 $2 }
svngrep () { grep -R $* | grep -v "^[^:]*\.svn/.*:" }

# Colorized ls
if [ `uname -s` = "Darwin" ]; then
   alias ls="ls -hGF"
else
   alias ls="ls -hF --color=auto"
fi
alias ll="ls -l"
alias la="ls -a"
alias lla="ls -la"

# Prompt
PROMPT="%{[0;33m%}%B%n:%~%#%{[0m%} "

# History
HISTFILE=~/.zsh_history
HISTSIZE=2048
SAVEHIST=2048

# Slash not a part of word
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# Watch users
#LOGCHECK=10
#watch=(all)

# Check mail
#MAILCHECK=10
#mailpath=(/home/alpha/mail/mbox/)

# Completion
autoload -U compinit
compinit

# Colors
which dircolors > /dev/null && eval `dircolors -b`
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Set Emacs style editing
bindkey -e

# Append to history file instantly
setopt incappendhistory

# Share history with other zshs
#setopt sharehistory

# Ignore entries starting with space in history
setopt histignorespace

# Ignore duplicates in history
setopt histignoredups

# Safe redirections
setopt noclobber

# Don't show completion menu
setopt noautomenu

# Show list of completions instead
setopt autolist

# Enter directories without cd
setopt autocd

# Enable spelling correction
setopt correct

# Set window title
xtitle() { print -Pn "\e]0;$1\a" }
stitle() { print -Pn "\ek$1\e\\" }

case $TERM in
    xterm*|rxvt)
        precmd() { xtitle "zsh %~" }
        preexec() { xtitle "$1" }
    ;;
   
    screen)
        precmd() { stitle "zsh %~" }
        preexec() { stitle "$1" }
    ;;
esac
