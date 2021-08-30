# Source setting files
if [[ -d "${ZDOTDIR:-$HOME/.zsh}" ]]; then
    source "${ZDOTDIR:-$HOME/.zsh/alias.zsh}"
    source "${ZDOTDIR:-$HOME/.zsh/export.zsh}"
    source "${ZDOTDIR:-$HOME/.zsh/functions.zsh}"
    source "${ZDOTDIR:-$HOME/.zsh/plugin.zsh}"
    source "${ZDOTDIR:-$HOME/.zsh/option.zsh}"
fi

