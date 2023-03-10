.PHONY: stow unstow restow submodule docker

stow: submodule
	@# Avoid bundling certain directories
	@mkdir -p ${HOME}/.config ${HOME}/.vim/pack/minpac/opt
	stow -vS .

unstow:
	stow -vD .

restow: unstow stow

submodule:
	git submodule update --init

docker: submodule
	docker build -t dotfiles .
