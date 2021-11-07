default: stow

stow:
	@mkdir -p ${HOME}/.config
	stow -v -S .

unstow:
	stow -v -D .

restow: unstow stow
