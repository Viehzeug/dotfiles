
# # enable VCS systems you use
#zstyle ':vcs_info:*' enable git svn
#zstyle ':vcs_info:*:prompt:*' check-for-changes true

setopt prompt_subst
autoload -U promptinit
promptinit


# Look at http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#Version-Control-Information
# for mor options
#zstyle ':vcs_info:*' check-for-changes true
#zstyle ':vcs_info:*' unstagedstr '*'   # display this when there are unstaged changes
#zstyle ':vcs_info:*' stagedstr '+'  # display this when there are staged changes
#zstyle ':vcs_info:*' actionformats '[%b%|%a%c%u%]%f'
#zstyle ':vcs_info:*' formats ':%b%c%u%f'


#zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
#precmd () { vcs_info }



#PROMPT='[%n@%m %~${vcs_info_msg_0_}] %{$reset_color%}'

#my stuff
ZSH_THEME_GIT_PROMPT_PREFIX=" [%{%F{blue}%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{%f%k%b%F{green}%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{%F{red}%}*%{%f%k%b%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function _prompt_char() {
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    echo "%{%F{blue}%}±%{%f%k%b%}"
  else
    echo ' '
  fi
}

PROMPT='%{%F{green}%}%n%{%F{blue}%}@%{%F{cyan}%}%m%{%F{green}%} %{%b%F{yellow}%}%~%{%F{green}%}$(git_prompt_info)%E%{%f%k%b%}$(_prompt_char) %#%{%f%k%b%} '
