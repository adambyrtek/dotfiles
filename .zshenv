# ~/.zshenv

# Initialize Fink (Mac OS X)
if [ -r /sw/bin/init.sh ]; then
   source /sw/bin/init.sh 2> /dev/null
fi

# Set environment
#export PATH=$PATH":/sbin:/usr/sbin:/usr/games/bin:$HOME/bin:/usr/local/bin:/usr/local/sbin:."
export LESS="-R -X -M"
export PAGER=less
export EDITOR=vim
export MANPAGER="col -b | view -c 'set ft=man nomod nolist' -"
export LANG=en_US.UTF-8
