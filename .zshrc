# Antigen plugin manager
if [ -f ~/.zsh/antigen.zsh ]; then
    source ~/.zsh/antigen.zsh

    # OMZ bundles
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
    antigen theme robbyrussell

    # Apply config
    antigen apply
else
    echo "Antigen not found!"
fi

# Zsh settings
setopt no_share_history

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

# OMZ universal clipboard aliases
type clipcopy > /dev/null && alias pbcopy='clipcopy'
type clippaste > /dev/null && alias pbpaste='clippaste'

# Find process by name or port
function psgrep() { pgrep -f "$*" | xargs ps -p }
function port() { lsof -s TCP:LISTEN -i TCP:${1:-0} }

# Load NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# Load RVM
[ -s "$HOME/.rvm/scripts/rvm" ] && source "$HOME/.rvm/scripts/rvm"
