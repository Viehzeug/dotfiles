
## pager
export PAGER="less"
export LESS="-R"

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_CTYPE=$LANG

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Code
source /usr/local/bin/virtualenvwrapper.sh


export HOMEBREW_GITHUB_API_TOKEN=dc456ee5196ed845df2ee952870e45bf19286c65

export PATH=/Developer/NVIDIA/CUDA-8.0/bin:${PATH}
export DYLD_LIBRARY_PATH=/Developer/NVIDIA/CUDA-8.0/lib\ :${DYLD_LIBRARY_PATH}

export EDITOR='emacsclient -nw'
alias e=$EDITOR
alias eg='emacsclient -n'
alias vi=$EDITOR

. /Users/marc/torch/install/bin/torch-activate
