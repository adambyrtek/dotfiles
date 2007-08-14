# ~/.zshrc
# Sourced for interactive shells

# Initialize Fink (Mac OS X)
if [ -r /sw/bin/init.sh ]; then
   source /sw/bin/init.sh 2> /dev/null
fi

# Aliases

# Debian
if [ -e /usr/bin/aptitude ]; then 
   alias a=aptitude
   alias sa="sudo aptitude"
else
   alias a=apt-get
   alias sa="sudo apt-get"
fi

# Gentoo
if [ -e /usr/bin/emerge ]; then
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

if [ `uname -s` = "Darwin" ]; then
   alias ls="ls -hGF"
else
   alias ls="ls -hF --color=auto"
fi

# Appliation aliases
alias sc=sitecopy
alias jedgrep="jed -f grep"

# Variables

PROMPT="%B%/%#%b "
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

if [ -x /usr/bin/dircolors ]; then
   eval `/usr/bin/dircolors -b`
fi
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# ZSH options

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

# Set window title

xtitle () { print -Pn "\e]0;$1\a" }
stitle () { print -Pn "\ek$1\e\\" }

case $TERM in
   xterm*|rxvt*)
      precmd () { xtitle "zsh %/" }
      preexec () { xtitle "$1" }
   ;;
   
   screen)
      precmd () { stitle "zsh" }
      preexec () 
      {
         local -a cmd; cmd=(${(z)1})
         stitle "$cmd[1]"
      }
   ;;
esac

# Startup

if [ -e /usr/bin/pal ]; then
   pal
fi
