default: stow

stow:
	@# Avoid bundling certain directories
	@mkdir -p ${HOME}/.config ${HOME}/.vim/pack/minpac/opt
	stow -v -S .

unstow:
	stow -v -D .

restow: unstow stow
