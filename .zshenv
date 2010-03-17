# ~/.zshenv
# Sourced for all shells

# Detect Mac OS X
if [[ -n $SECURITYSESSIONID ]]; then
    export MACOSX=1
fi

# Basic environment
export PATH="$PATH:/usr/local/bin:$HOME/bin:$HOME/.python/bin"
export PAGER=less
export LESS="-R -X -M -i -S"
export EDITOR=vim
export VISUAL=$EDITOR
export LANG=en_US.UTF-8
export GREP_OPTIONS="--color=auto -i"
export GREP_COLOR=32
export EMAIL="adambyrtek@gmail.com"

# Ruby gems installed in home directory, always require rubygems
export RUBYOPT="-rubygems"
export GEM_HOME="$HOME/.gems"
export PATH="$PATH:$GEM_HOME/bin"

# Startup file for Python interpreter
export PYTHONSTARTUP="$HOME/.pythonrc.py"

# Current directory always at the end
export PATH="$PATH:."

# MacPorts
if [[ -n $MACOSX && -d /opt/local ]]; then
    export PATH=/opt/local/bin:/opt/local/sbin:/opt/local/lib/postgresql83/bin:$PATH
    export MANPATH=/opt/local/share/man:$MANPATH
    export DISPLAY=:0.0
fi

# Java on Mac OS X
if [[ -n $MACOSX ]]; then
    export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home
    export ANT_HOME=/Developer/Java/ant
fi
