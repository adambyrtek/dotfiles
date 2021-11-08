FROM ubuntu:20.04

RUN apt-get update && apt-get upgrade -y

RUN apt-get install -y \
    curl \
    make \
    python3 \
    python3-pip \
    stow \
    zsh \
    tmux \
    neovim \
    git \
    hub \
    ripgrep \
    tree \
    lsof \
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
