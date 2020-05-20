# Force symbolic links, do not follow directories
ln=ln -fsT


default:
	echo "NO DEFAULT RULE"

git:
	sudo apt-get install -y git hub
	$(ln) ${PWD}/.gitconfig ${HOME}/.gitconfig

zsh: git
	sudo apt-get install -y zsh
	$(ln) ${PWD}/.zlogin ${HOME}/.zlogin
	$(ln) ${PWD}/.zsh ${HOME}/.zsh
	$(ln) ${PWD}/.zshrc ${HOME}/.zshrc
	echo "PLEASE RESTART THE TERMINAL"

python3:
	sudo apt-get install -y python3 python3-pip python3-virtualenv

vim: python3
	sudo apt-get install -y xclip silversearcher-ag neovim
	pip3 install pynvim
	$(ln) ${PWD}/.vim ${HOME}/.vim
	$(ln) ${PWD}/.vimrc ${HOME}/.vimrc
	mkdir -p ${HOME}/.config
	$(ln) ${PWD}/.config/nvim ${HOME}/.config/nvim

alacritty: zsh
	sudo apt-get install -y alacritty
	mkdir -p ${HOME}/.config
	$(ln) ${PWD}/.config/alacritty ${HOME}/.config/alacritty

tmux:
	sudo apt-get install -y tmux
	$(ln) ${PWD}/.tmux.conf ${HOME}/.tmux.conf
	$(ln) ${PWD}/.tmux-status.conf ${HOME}/.tmux-status.conf

psql:
	sudo apt-get install -y postgresql-server-dev-all postgresql-client
	$(ln) ${PWD}/.psqlrc ${HOME}/.psqlrc

docker:
	sudo apt-get install -y docker.io

asdf: git zsh
	git clone https://github.com/asdf-vm/asdf.git ${HOME}/.asdf --branch v0.7.8
	$(ln) ${PWD}/.asdfrc ${HOME}/.asdfrc

asdf-ruby: asdf
	sudo apt-get install -y libssl-dev libreadline-dev
	zsh -c "asdf plugin add ruby"

tools:
	sudo apt-get install -y awscli httpie autossh
