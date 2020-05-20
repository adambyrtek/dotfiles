FROM ubuntu:20.04

RUN apt-get update && apt-get upgrade -y && apt-get install make sudo

WORKDIR /root
COPY . dotfiles/
RUN cd dotfiles && make zsh vim tmux

CMD ["zsh", "--login"]
