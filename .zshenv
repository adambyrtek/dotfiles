# ~/.zshenv

# Detect Mac OS X
if [[ -n $SECURITYSESSIONID ]]; then
    export MACOSX=1
fi

# Basic environment
export PATH="$PATH:/usr/local/bin:$HOME/bin:."
export PAGER=less
export LESS="-R -X -M -I -S"
export EDITOR=vim
export VISUAL=$EDITOR
export MANPAGER="col -b | view -c 'set ft=man nomod nolist' -"
export LANG=en_US.UTF-8
export GREP_OPTIONS="--color=auto -i"
export GREP_COLOR=32
export EMAIL="adambyrtek@gmail.com"

# MacPorts
if [[ -n $MACOSX && -d /opt/local ]]; then
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export MANPATH=/opt/local/share/man:$MANPATH
    export DISPLAY=:0.0
fi

# Enable lesspipe if present
if which lesspipe.sh > /dev/null; then
    export LESSOPEN="|$(which lesspipe.sh) %s"
fi

# Java on Mac OS X
if [[ -n $MACOSX ]]; then
    export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home
    export ANT_HOME=/Developer/Java/ant
fi
