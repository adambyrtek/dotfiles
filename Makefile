.PHONY: stow unstow restow

stow:
	@# Avoid bundling certain directories
	@mkdir -p ${HOME}/.config ${HOME}/.vim/pack/minpac/opt
	stow -v -S .

unstow:
	stow -v -D .

restow: unstow stow

docker:
	docker build -t dotfiles .
