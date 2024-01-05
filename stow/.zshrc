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

export PATH="${HOME}/.local/bin:${PATH}"
export PATH="/opt/homebrew/bin:$PATH"
. /opt/homebrew/etc/profile.d/z.sh

for config_file ($ZSH/lib/*.zsh) source $config_file

# Load and run compinit
autoload -U compinit
compinit -i

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
export MODULAR_HOME="/Users/marc/.modular"
export PATH="/Users/marc/.modular/pkg/packages.modular.com_mojo/bin:$PATH"
