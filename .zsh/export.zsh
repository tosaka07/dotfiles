# -------------------------------
# Export
# -------------------------------

# Vim
export XDC_CONFIG_HOME="$HOME/.config"

# fzf
zle -N ghql
bindkey '^G' ghql

zle -N fzf-checkout-branch
bindkey '^B' fzf-checkout-branch

# ENV
source "$HOME/dotfiles/ENV.sh"

# n
export N_PREFIX="$HOME/n"
export PATH="$PATH:$N_PREFIX/bin"

# jenv
# export PATH="$HONE/.jenv/bin:$PATH"
# eval "$(jenv init -)"

# rbenv
eval "$(rbenv init -)"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
