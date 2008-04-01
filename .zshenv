# ~/.zshenv

# Detect Mac OS X
if [[ -n $SECURITYSESSIONID ]]; then
    export MACOSX=1
fi

# Initialize Fink (Mac OS X)
if [[ -n $MACOSX && -r /sw/bin/init.sh ]]; then
   source /sw/bin/init.sh 2> /dev/null
fi

# Basic environment
export PATH="$PATH:$HOME/bin:/var/lib/gems/1.8/bin:/usr/local/bin:/usr/local/sbin:."
export PAGER=less
export LESS="-R -X -M -I"
export EDITOR=vim
export VISUAL=$EDITOR
export MANPAGER="col -b | view -c 'set ft=man nomod nolist' -"
export LANG=en_US.UTF-8
export GREP_OPTIONS="--color=auto"
export GREP_COLOR=32

# MacPorts
if [[ -n $MACOSX && -d /opt/local ]]; then
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export MANPATH=/opt/local/share/man:$MANPATH
    export DISPLAY=:0.0
fi

# Java on Mac OS X
if [[ -n $MACOSX ]]; then
    # Java on Mac OS X
    export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Home"
    export ANT_HOME="/Developer/Java/ant"

    # JRuby
    if [[ -d "$HOME/Code/jruby" ]]; then
        export JRUBY_HOME="$HOME/Code/jruby"
    fi
fi

# JRuby
if [[ -n $JRUBY_HOME ]]; then
    export PATH="$PATH:$JRUBY_HOME/bin"
    export CLASSPATH="lib/jruby.jar:lib/asm-3.0.jar:lib/jna.jar:."
fi
