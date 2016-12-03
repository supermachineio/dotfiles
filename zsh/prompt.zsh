autoload colors; colors;
setopt prompt_subst

GIT_PROMPT='%{$fg[magenta]%}$(git_prompt_info)%{$reset_color%}'
PROMPT="%{$fg[green]%}[%~] $GIT_PROMPT%{$reset_color%} "
RPS1="$GIT_PROMPT"

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""

# show VI insert mode
function zle-line-init zle-keymap-select {
  VIM_MODE="%{$fg[yellow]%} [% NORMAL]% %{$reset_color%}"
  RPS1="${${KEYMAP/vicmd/$VIM_MODE}/(main|viins)/} $EPS1"
  zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
