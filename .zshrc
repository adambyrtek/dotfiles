# .zshrc is sourced for interactive shells only

# Antigen plugin manager
if [[ -r ~/.zsh/antigen.zsh ]]; then
    source ~/.zsh/antigen.zsh

    # OMZ plugins
    antigen bundle ohmyzsh/ohmyzsh
    antigen bundle ohmyzsh/ohmyzsh plugins/command-not-found
    antigen bundle ohmyzsh/ohmyzsh plugins/git

    # OMZ completion plugins
    antigen bundle ohmyzsh/ohmyzsh plugins/pip
    antigen bundle ohmyzsh/ohmyzsh plugins/terraform

    # Other plugins
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-syntax-highlighting
    # antigen bundle zsh-users/zsh-autosuggestions

    antigen bundle redxtech/zsh-asdf-direnv

    # Prompt
    zstyle :prompt:pure:git:stash show yes
    antigen bundle mafredri/zsh-async
    antigen bundle sindresorhus/pure@main

    antigen apply
else
    echo "Antigen not found!"
fi

# Separate history for each shell
setopt no_share_history

# Highlight isearch match
zle_highlight=("isearch:bg=yellow,fg=black")

# Fix Shift/Alt with arrow keys
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word
bindkey "^[[1;2C" forward-char
bindkey "^[[1;2D" backward-char

# Custom paths
path=("$HOME/Dev/bin" "$HOME/.rvm/bin" "$HOME/.local/bin" $path)
cdpath=("$HOME/Dev" $cdpath)
export PATH CDPATH

# Environment variables
export EDITOR="vim"
export BROWSER="xdg-open"
export LESS="-FSRXMi"
export PYTHONDONTWRITEBYTECODE=1

# Common aliases
alias a="apt"
alias sa="sudo apt"
alias py="python3"
alias o="xdg-open"
alias l="less"
alias g="grep -E"

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

# OMZ history with date and time
alias h="omz_history -i"

# Grep running processes
alias psg="pgrep -fa"

# Show open ports
alias ports="lsof -s TCP:LISTEN -i TCP"

# External IP address
alias myip="curl https://ifconfig.me"

# Login message
[[ -o login ]] && echo "Hello world!"
