# .zshrc is sourced for interactive shells only

# Antigen plugin manager
if [[ -r ~/.zsh/antigen.zsh ]]; then
    source ~/.zsh/antigen.zsh

    # OMZ plugins
    zstyle ':omz:update' mode disabled
    antigen bundle ohmyzsh/ohmyzsh
    antigen bundle ohmyzsh/ohmyzsh plugins/command-not-found
    antigen bundle ohmyzsh/ohmyzsh plugins/git

    # OMZ completion plugins
    antigen bundle ohmyzsh/ohmyzsh plugins/pip
    antigen bundle ohmyzsh/ohmyzsh plugins/terraform

    # Other plugins
    antigen bundle redxtech/zsh-asdf-direnv
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-syntax-highlighting

    # Pure prompt with Git stash enabled
    zstyle :prompt:pure:git:stash show yes
    antigen bundle mafredri/zsh-async
    antigen bundle sindresorhus/pure@main

    antigen apply
else
    echo "Antigen not found!"
fi

# Separate history for each shell
setopt no_share_history

# Incrementaly append to the history file
setopt inc_append_history

# Avoid accidentally closing the shell with Ctrl-D
setopt ignore_eof

# Highlight isearch match
zle_highlight=("isearch:bg=yellow,fg=black")

# List of unique paths
typeset -U path cdpath
path=("$HOME/Dev/bin" "$HOME/.local/bin" $path)
cdpath=("$HOME/Dev" $cdpath)
export PATH CDPATH

# Environment variables
export EDITOR="vim"
export BROWSER="xdg-open"
export LESS="-FSRXMi"
export PYTHONDONTWRITEBYTECODE=1

# Common aliases
alias l="less"
alias g="grep -E"
alias o="xdg-open"
alias a="apt"
alias sa="sudo apt"
alias py="python3"
alias gs="git status -s"

# Safer file operations
alias mv="mv -i"
alias rm="rm -i"

# Use Neovim instead of Vim if present
if type nvim > /dev/null; then
    export EDITOR="nvim"
    alias vim="nvim"
fi

# Tmux aliases
alias ta="tmux attach -t"
alias tns="tmux new-session -s"
alias tls="tmux list-sessions"

# OMZ clipboard aliases
type clipcopy > /dev/null && alias pbcopy="clipcopy"
type clippaste > /dev/null && alias pbpaste="clippaste"

# Full history with date and time
alias hl="fc -il 1"

# Reload history from file
alias hr="fc -R"

# Grep running processes
alias psg="pgrep -fa"

# Show open ports
alias ports="lsof -s TCP:LISTEN -i TCP"

# External IP address
alias myip="curl https://ifconfig.me"

# Login message
[[ -o login ]] && echo "Hello world!"
