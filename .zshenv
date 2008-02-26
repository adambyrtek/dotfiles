# ~/.zshenv

# Initialize Fink (Mac OS X)
if [ -r /sw/bin/init.sh ]; then
   source /sw/bin/init.sh 2> /dev/null
fi

# Set environment
export PATH="$PATH:$HOME/bin:/var/lib/gems/1.8/bin:/usr/local/bin:/usr/local/sbin:."
export LESS="-R -X -M -I"
export PAGER=less
export EDITOR=vim
export VISUAL=$EDITOR
export MANPAGER="col -b | view -c 'set ft=man nomod nolist' -"
export LANG=en_US.UTF-8
export GREP_OPTIONS="--color=auto"
export GREP_COLOR=32
