# Antigen plugin manager
if [ -f ~/.zsh/antigen.zsh ]; then
    # TODO: Consider Antibody
    source ~/.zsh/antigen.zsh

    # OMZ bundles
    # TODO: Consider removing OMZ completely
    antigen use oh-my-zsh
    antigen bundle git
    antigen bundle command-not-found

    # OMZ completions
    antigen bundle pip
    antigen bundle terraform

    # External bundles
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-syntax-highlighting

    # Prompt
    # antigen theme robbyrussell
    # PURE_PROMPT_SYMBOL='λ'
    antigen bundle mafredri/zsh-async
    antigen bundle sindresorhus/pure

    # Apply config
    antigen apply
else
    echo "Antigen not found!"
fi

# Separate shell history
setopt no_share_history

# Highlight isearch match
zle_highlight=("isearch:bg=yellow,fg=black")

# Revert OMZ custom Up/Down bindings
bindkey "${terminfo[kcuu1]}" up-line-or-history
bindkey "${terminfo[kcud1]}" down-line-or-history

# Fix Shift/Alt with arrow keys
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word
bindkey "^[[1;2C" forward-char
bindkey "^[[1;2D" backward-char

# Custom paths
path+=("$HOME/Dev/bin" "$HOME/.rvm/bin" "$HOME/.local/bin")
cdpath+=("$HOME/Dev" "$HOME/Dev/airsorted")
export PATH CDPATH

# Environment variables
export EDITOR='vim'
export BROWSER='xdg-open'
export LESS='-FSRXMi'
export PYTHONDONTWRITEBYTECODE=1

# Aliases
alias a='apt'
alias sa='sudo apt'
alias s='sudo'
alias p='python3'
alias o='xdg-open'
alias l='less'
alias g='grep'
alias gg='gitg -c &> /dev/null &!'

# Use NeoVim instead of Vim if present
if type nvim > /dev/null; then
    export EDITOR='nvim'
    alias vim='nvim'
fi

# Tmux aliases
alias ta='tmux attach -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'

# Mac-like clipboard aliases
type clipcopy > /dev/null && alias pbcopy='clipcopy'
type clippaste > /dev/null && alias pbpaste='clippaste'

# Find process by name or port
function psgrep() { pgrep -f "$*" | xargs ps -p }
function port() { lsof -s TCP:LISTEN -i TCP:${1:-0} }

# Server current directory over HTTP
alias serve='python3 -m http.server 8000'

# Load NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# Load RVM
[ -s "$HOME/.rvm/scripts/rvm" ] && source "$HOME/.rvm/scripts/rvm"
