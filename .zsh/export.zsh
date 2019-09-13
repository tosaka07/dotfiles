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

# Colordiff
if hash 'colordiff'; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# jenv
# export PATH="$HONE/.jenv/bin:$PATH"
# eval "$(jenv init -)"

# rbenv
eval "$(rbenv init -)"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
export PATH=~/.local/bin:$PATH
