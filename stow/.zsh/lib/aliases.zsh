# ls
alias ll='ls -la'
alias lh='ls -lah'

# cd
alias ..='cd ..'
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'
alias cd/='cd /'

# editor
if (( $+commands[nvim] )); then
  alias vi='nvim'
  alias vim='nvim'
fi

# Super user
alias _='sudo'
alias please='sudo'
