export ZSH=~/.zsh

for config_file ($ZSH/lib/*.zsh) source $config_file

. /usr/local/etc/profile.d/z.sh

# Load and run compinit
autoload -U compinit
compinit -i
