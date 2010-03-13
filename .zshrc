# ~/.zshrc
# Sourced for interactive shells only

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

# Aliases
alias !=history
alias '...'='../..'
alias '....'='../../..'
alias '.....'='../../../../'
alias l="${PAGER:-less}"
alias e="${EDITOR:-vim}"
alias g=grep
alias s=screen
alias o=open
alias psa=ps aux
alias psgrep="ps aux | grep"
calc () { echo $* | bc -l }
vimgrep() { vim -c "vimgrep /$1/ $*[2,-1]" -c copen }
si() { sudo /etc/init.d/$1 $2 }
beep() { printf "\a" }

# Global aliases
alias -g '***'='**/*'

# Default parameters
alias dirs="dirs -v"
alias history="history -iD"
alias du="du -chs"
alias df="df -h"
alias pstree="pstree -h"
alias diff="diff -uN"
alias tree="tree -F"

# Emulate open command from Mac OS X
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

# Load colors module
autoload colors && colors

# Initialize VCS info module
autoload vcs_info

# Prompt inspired by http://kriener.org/articles/2009/06/04/zsh-prompt-magic
for COLOR in RED GREEN YELLOW WHITE BLACK CYAN; do
    eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
PR_RESET="%{${reset_color}%}";

FMT_BRANCH="${PR_GREEN}%b%u%c${PR_RESET}"
FMT_ACTION="(${PR_CYAN}%a${PR_RESET}%)"
FMT_PATH="%R${PR_YELLOW}/%S"

zstyle ':vcs_info:*:prompt:*' check-for-changes false
zstyle ':vcs_info:*:prompt:*' unstagedstr '?'
zstyle ':vcs_info:*:prompt:*' stagedstr '!'
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}//" "${FMT_PATH}"
zstyle ':vcs_info:*:prompt:*' formats "${FMT_BRANCH}//" "${FMT_PATH}"
zstyle ':vcs_info:*:prompt:*' nvcsformats "" "%~"

function lprompt {
    local brackets=$1
    local color1=$2
    local color2=$3

    local bracket_open="${color1}${brackets[1]}${PR_RESET}"
    local bracket_close="${color1}${brackets[2]}"

    local git='$vcs_info_msg_0_'
    local cwd="${color2}%B%1~%b"

    PROMPT="${PR_RESET}${bracket_open}${git}${cwd}${bracket_close}%# ${PR_RESET}"
}

function rprompt {
    local brackets=$1
    local color1=$2
    local color2=$3

    local bracket_open="${color1}${brackets[1]}${PR_RESET}"
    local bracket_close="${color1}${brackets[2]}${PR_RESET}"
    local colon="${color1}:"
    local at="${color1}@${PR_RESET}"

    local user_host="${color2}%n${at}${color2}%m"
    local vcs_cwd='${${vcs_info_msg_1_%%.}/$HOME/~}'
    local cwd="${color2}%B%30<..<${vcs_cwd}%<<%b"
    local inner="${user_host}${colon}${cwd}"

    RPROMPT="${PR_RESET}${bracket_open}${inner}${bracket_close}${PR_RESET}"
}

lprompt '[]' $BR_BRIGHT_BLACK $PR_WHITE
rprompt '()' $BR_BRIGHT_BLACK $PR_WHITE

# Spelling correction prompt
SPROMPT="%{${bg[red]}%}zsh: correct '%R' to '%r' [nyae]?%{${reset_color}%} "

# History file
HISTFILE=~/.zsh_history

# Size of internal history buffer
HISTSIZE=1000000

# Number of history lines to save
SAVEHIST=1000000

# Show as many completions as fit on the screen
LISTMAX=0

# Slash not a part of a word
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# Set Emacs style editing
bindkey -e

# Ctrl-Left/Right
bindkey '\e[1;5D' backward-word
bindkey '\e[1;5C' forward-word

# Ctrl-P/Ctrl-N
bindkey "^N" history-beginning-search-forward
bindkey "^P" history-beginning-search-backward

# Enable completion
autoload -U compinit && compinit

# Colors in completion
if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

# Show completion indicator
zstyle ":completion:*" show-completer true

# Grouping for completion types
zstyle ':completion:*:descriptions' format "%{${fg[magenta]}%}-* %d *-%{$reset_color%}"
zstyle ':completion:*' group-name ""

# Split manual pages by sections
zstyle ':completion:*:manuals' separate-sections 'yes'

# Ignore internal zsh functions
zstyle ':completion:*:functions' ignored-patterns '_*'

# Describe all command options
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d*'

# Process completion shows all processes, has menu and colors
zstyle ':completion:*:*:*:*:processes' command  'ps -A -o pid,user,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'

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

# Print the exit value for commands with non-zero exit status
setopt printexitvalue

# Set screen or xterm title
title() {
    # Truncate long command and join lines
    t=$(print -Pn "%40>...>$1" | tr -d "\n")

    case $TERM in
    screen)
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
precmd() { 
    title "zsh %1~"
    vcs_info prompt
}

# Hook run before executing command
preexec() { title "$1" }

# Hook run on each directory change
chpwd() { ls }

# man pages displayed in vim
man() { /usr/bin/man $* | col -b | vim -R -c 'set ft=man nomod nolist' -; }

# Enable lesspipe if present
if which lesspipe > /dev/null; then
    eval $(lesspipe)
fi
