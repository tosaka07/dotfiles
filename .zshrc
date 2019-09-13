# zmodload zsh/zprof && zprof

# Starship
eval "$(starship init zsh)"

# Source Zshrc.
if [[ -d "${ZDOTDIR:-$HOME/.zsh}" ]]; then
  source "${ZDOTDIR:-$HOME/.zsh/alias.zsh}"
  source "${ZDOTDIR:-$HOME/.zsh/functions.zsh}"
  source "${ZDOTDIR:-$HOME/.zsh/plugin.zsh}"
  source "${ZDOTDIR:-$HOME/.zsh/setopt.zsh}"
  source "${ZDOTDIR:-$HOME/.zsh/export.zsh}"
  source "${ZDOTDIR:-$HOME/.zsh/completion.zsh}"
fi

# if (which zprof > /dev/null 2>&1) ;then
#   zprof
# fi
