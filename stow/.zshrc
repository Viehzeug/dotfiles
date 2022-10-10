export ZSH=~/.zsh


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
if type nvim > /dev/null; then
    export EDITOR='nvim'
    alias eg='nvim'
else
    export EDITOR='vi'
    alias eg='vi'
fi

export PATH="${HOME}/.local/bin:${PATH}"

alias e=$EDITOR

export GUROBI_HOME="/opt/gurobi811/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"





for config_file ($ZSH/lib/*.zsh) source $config_file

. ~/.z.sh

# Load and run compinit
autoload -U compinit
compinit -i

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


export PATH="$HOME/.rbenv/shims:$PATH"
