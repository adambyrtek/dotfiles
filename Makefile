.PHONY: stow unstow restow docker

stow:
	@# Avoid bundling certain directories
	@mkdir -p ${HOME}/.config ${HOME}/.vim/pack/minpac/opt
	stow -vS .

unstow:
	stow -vD .

restow: unstow stow

docker:
	git submodule update --init
	docker build -t dotfiles .
