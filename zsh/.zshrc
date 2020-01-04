export ZSH=~/.zsh

for config_file ($ZSH/lib/*.zsh) source $config_file

. ~/.z.sh

# Load and run compinit
autoload -U compinit
compinit -i

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/marc/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/marc/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/marc/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/marc/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

