# get the name of the branch we are on
function git_prompt_info() {
  _in_git_repo=0
  ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
  _in_git_repo=1
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# Checks if working tree is dirty
parse_git_dirty() {
  local GIT_STATUS=''
  GIT_STATUS=$(command git status -s --ignore-submodules=dirty 2> /dev/null | tail -n1)
  if [[ -n $GIT_STATUS ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}
