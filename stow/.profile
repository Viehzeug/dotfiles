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
