## pager
if type less > /dev/null; then
    export PAGER="less"
    export LESS="-R"
fi
    
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=$LANG


if [[ $OSTYPE == darwin* ]]; then
    source ~/.brew_github_key
elif [[ $OSTYPE == linux-gnu ]]; then
    function open () {
        xdg-open "$*" >/dev/null 2>/dev/null & 
    }
fi

## editor
## if we have emacs, use emacs - else vi(m)
if type emacs > /dev/null; then
    export EDITOR='emacsclient -nw'
    alias eg='emacsclient -n'
else
    export EDITOR='vi'
    alias eg='vi'
fi

alias e=$EDITOR

export GUROBI_HOME="/opt/gurobi811/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"
