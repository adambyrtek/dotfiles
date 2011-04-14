# vim:fdm=marker

# ~/.zshrc
# Sourced for interactive shells only

# {{{ Environment variables

# Preload color variables
autoload colors && colors

# Unique path array
typeset -U path
path=($path /usr/local/bin $HOME/bin $HOME/.python/bin)

# Detect Mac OS X
if [[ $(uname) == "Darwin" ]]; then
    export IS_MACOSX="1"
fi
if [[ $(uname) == "FreeBSD" ]]; then
    export IS_FREEBSD="1"
fi

# Basic environment
export PAGER="less"
export LESS="-R -X -M -i -S"
export EDITOR="vim"
export VISUAL="$EDITOR"
export LANG="en_US.UTF-8"
export GREP_OPTIONS="--color=auto"
export GREP_COLOR="1;33"
export EMAIL="adambyrtek@gmail.com"

# Ruby gems installed in home directory
export GEM_HOME="$HOME/.gems"
export RB_USER_INSTALL="1"
path=($path "$GEM_HOME/bin")

# Startup file for the Python interpreter
export PYTHONSTARTUP="$HOME/.pythonrc.py"

# Current directory always at the end
path=($path .)

# MacPorts
if [[ -n $IS_MACOSX && -d /opt/local ]]; then
    export PATH="/opt/local/bin:/opt/local/sbin:/opt/local/lib/postgresql83/bin:$PATH"
    export MANPATH="/opt/local/share/man:$MANPATH"
    export DISPLAY=":0.0"
fi

# Java on Mac OS X
if [[ -n $IS_MACOSX ]]; then
    export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Home"
    export ANT_HOME="/Developer/Java/ant"
fi

# }}}
# {{{ Aliases and public functions

alias !=history
alias ...='../..'
alias ....='../../..'
alias .....='../../../../'

alias l="${PAGER:-less}"
alias e="${EDITOR:-vim}"
alias g=grep
alias s=screen
alias o=open
alias x="dtrx -v --one=here"
d() { dict $* | less }

alias psa="ps aux"
psgrep() { ps aux | grep $* }
calc() { echo $* | bc -l }
vimgrep() { vim -c "vimgrep /$1/ $*[2,-1]" -c copen }
beep() { printf "\a" }
webshare() { python -m SimpleHTTPServer $* }

# Default parameters
alias dirs="dirs -v"
alias history="history -iD"
alias du="du -chs"
alias df="df -h"
alias pstree="pstree -h"
alias diff="diff -uN"
alias tree="tree -F"

# Emulate useful Mac OS X commands
if which xdg-open > /dev/null; then
    alias open="xdg-open"
fi
if which xclip > /dev/null; then
    alias pbcopy="xclip -i"
    alias pbpaste="xclip -o"
fi

# Sync history from disk
alias h="fc -R"

# Colorized ls
if which dircolors > /dev/null; then
    eval $(dircolors -b)
fi
if [[ -n $IS_MACOSX || -n $IS_FREEBSD ]]; then
    # BSD has its own way
    alias ls="ls -hGF"
else
    alias ls="ls -hF --color=auto"
fi
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -lA"

# cd lists the new directory
cd() { builtin cd $* && ls }

# Man pages displayed in vim
if which vim > /dev/null; then
    man() {
        /usr/bin/man $* | \
            col -b | \
            vim -R -c 'set ft=man nomod nolist' -
    }
fi

# Ack on Debian is called ack-grep
if which ack-grep > /dev/null; then
  alias ack=ack-grep
fi

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

# Package management for Arch Linux
if which pacman > /dev/null; then
    alias p=pacman
    alias sp="sudo pacman"
fi

# Package management for MacPorts
if which port > /dev/null; then
    alias p=port
    alias sp="sudo port"
fi

# Global aliases
alias -g '***'='**/*'

# }}}
# {{{ Zsh variables

# Autoload all local functions
fpath=(~/.zsh/functions $fpath)
autoload -U "" ~/.zsh/functions/*(N:t)

# Enable registration of hooks
typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

# History file
HISTFILE=~/.zsh_history

# Size of internal history buffer
HISTSIZE=1000000

# Number of history lines to save
SAVEHIST=1000000

# Show as many completions as fit on the screen
LISTMAX=0

# Slash should not be treated as part of a word
WORDCHARS="${WORDCHARS:s#/#}"

# Report time for commands taking more than a minute
REPORTTIME=60

# Spelling correction prompt
SPROMPT="%{${fg_bold[red]}%}zsh: correct '%R' to '%r' [nyae]?%{${reset_color}%} "

# }}}
# {{{ Key and ZLE bindings

# Set Emacs style editing
bindkey -e

# Ctrl-Left/Right behave like in Bash
bindkey '\e[1;5D' backward-word
bindkey '\e[1;5C' forward-word

# Edit command line in external editor
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

# Quote URLs pasted on the command line
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# }}}
# {{{ Completion

# Enable completion
autoload -U compinit && compinit

# Display message when no matches are found
zstyle ':completion:*:warnings' format "%{${fg_bold[yellow]}%}zsh: no matches for%{$reset_color%} %d"

# Colors in completion
if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

# Smart case matching
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Grouping for completion types
zstyle ':completion:*:descriptions' format "%{${fg_bold[magenta]}%}= %d =%{$reset_color%}"
zstyle ':completion:*' group-name ""

# Split manual pages by sections
zstyle ':completion:*:manuals' separate-sections 'yes'

# Ignore internal zsh functions
zstyle ':completion:*:functions' ignored-patterns '_*'

# Describe all command options
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d*'
zstyle ':completion:*:options' list-separator '#'

# Process completion shows all processes with colors
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:*:*:*:processes' menu yes select
zstyle ':completion:*:*:*:*:processes' command 'ps -A -o pid,user,cmd'
zstyle ':completion:*:*:*:*:processes' list-colors "=(#b) #([0-9]#)*=0=${color[green]}"

# List all processes for killall
zstyle ':completion:*:processes-names' command "ps -eo cmd= | sed 's:\([^ ]*\).*:\1:;s:\(/[^ ]*/\)::;/^\[/d'"

# Cache expensive completions
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh

# }}}
# {{{ Zsh options

# Append to history file instantly
setopt incappendhistory

# Save timestamps in history file
setopt extendedhistory

# Ignore entries starting with space in history
setopt histignorespace

# Ignore duplicates in history
setopt histignoredups

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

# Ignore duplicates on the stack
setopt pushdignoredups

# Spelling correction
setopt correct

# No beep when showing list of completions
setopt nolistbeep

# No line editor beep at all
setopt nobeep

# Dynamic variable substitution in prompt
setopt promptsubst

# Erase the right prompt after a line is accepted
setopt transientrprompt

# Print the exit value for commands with non-zero exit status
setopt printexitvalue

# }}}
# {{{ Prompt and title

# Prompt defined in a separate function
vcs_prompt

# Set screen or xterm title
title() {
    # Truncate long command and join lines
    t=$(print -Pn "%40>...>$1" | tr -d "\n")

    case $TERM in
        screen*)
            # Update screen title
            print -Pn "\ek$t\e\\"
            ;;
        xterm*|rxvt)
            # Update xterm window title
            print -Pn "\e]0;$t\a"
            ;;
    esac
}

# Hook run before showing prompt
precmd_functions+=precmd_title
precmd_title() { title "zsh %1~" }

# Hook run before executing command
preexec_functions+=preexec_title
preexec_title() { title "$1" }

# }}}
# {{{ Extra initialization

# Enable lesspipe if present
if which lesspipe > /dev/null; then
    eval $(lesspipe)
fi

# }}}
