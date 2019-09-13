# -------------------
# Plugin
# -------------------

# Zplugin
source "$HOME/.zplugin/bin/zplugin.zsh"
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

# List

zplugin ice wait'!0'; zplugin load zsh-users/zsh-completions
zplugin ice wait'!0'; zplugin load zsh-users/zsh-syntax-highlighting
