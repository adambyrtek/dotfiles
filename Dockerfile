FROM ubuntu:20.04

RUN apt-get update && apt-get upgrade -y

# Install system dependencies
RUN apt-get install -y \
    make \
    python3 \
    python3-pip

# Install essential tools
RUN apt-get install -y \
    zsh \
    tmux \
    neovim \
    stow \
    git \
    hub \
    ripgrep \
    tree \
    autossh \
    httpie \
    jq

# Neovim Python integration
RUN pip3 install --no-cache-dir pynvim

WORKDIR /root
COPY . dotfiles/

# Symlink dotfiles
RUN cd dotfiles && make stow

CMD ["zsh", "--login"]
