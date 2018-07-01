export ZSH=~/.zsh

for config_file ($ZSH/lib/*.zsh) source $config_file

. /usr/local/etc/profile.d/z.sh

# Load and run compinit
autoload -U compinit
compinit -i


. /Users/marc/torch/install/bin/torch-activate

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/marc/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/marc/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/marc/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/marc/google-cloud-sdk/completion.zsh.inc'; fi
