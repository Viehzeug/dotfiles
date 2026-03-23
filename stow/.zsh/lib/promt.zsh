setopt prompt_subst
autoload -U promptinit
promptinit

ZSH_THEME_GIT_PROMPT_PREFIX=" [%{%F{blue}%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{%f%k%b%F{green}%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{%F{red}%}*%{%f%k%b%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function _prompt_char() {
  if (( _in_git_repo )); then
    echo "%{%F{blue}%}±%{%f%k%b%}"
  else
    echo ' '
  fi
}

PROMPT='%{%F{green}%}%n%{%F{blue}%}@%{%F{cyan}%}%m%{%F{green}%} %{%b%F{yellow}%}%~%{%F{green}%}$(git_prompt_info)%E%{%f%k%b%}$(_prompt_char) %#%{%f%k%b%} '
