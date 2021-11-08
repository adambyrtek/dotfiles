FROM ubuntu:21.10

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get upgrade -y

RUN apt-get install -y \
    curl \
    make \
    stow \
    zsh \
    tmux \
    neovim \
    python3-pynvim \
    git \
    hub \
    ripgrep \
    tree \
    lsof \
    autossh \
    httpie \
    jq

WORKDIR /root
COPY . dotfiles/

# Symlink dotfiles
RUN cd dotfiles && make stow

CMD ["zsh", "--login"]
