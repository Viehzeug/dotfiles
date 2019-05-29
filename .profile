## pager
if type less > /dev/null; then
    export PAGER="less"
    export LESS="-R"
fi
    
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=$LANG

if [[ $OSTYPE == darwin* ]]; then
    #on mac do this
    source ~/.brew_github_key
    export PATH=/Developer/NVIDIA/CUDA-8.0/bin:${PATH}
    export DYLD_LIBRARY_PATH=/Developer/NVIDIA/CUDA-8.0/lib\ :${DYLD_LIBRARY_PATH}
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
