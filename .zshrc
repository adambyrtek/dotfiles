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

# Custom paths
path+=("$HOME/Dev/bin" "$HOME/.rvm/bin" "$HOME/.local/bin")
cdpath+=("$HOME/Dev" "$HOME/Dev/airsorted")
export PATH CDPATH

# Environment
export EDITOR='vim'
export BROWSER='xdg-open'
export LESS='-R -X -M -i -S'
export PYTHONDONTWRITEBYTECODE=1

# Aliases
alias a='apt'
alias sa='sudo apt'
alias s='sudo'
alias p='python3'
alias o='xdg-open'
alias l='less'
alias g='grep'
alias vim='nvim'

alias ta='tmux attach -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'

# Do not share history between sessions
setopt no_share_history

# Load NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"

# Load RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
