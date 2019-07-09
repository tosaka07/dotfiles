export LANG=ja_JP.UTF-8

# Common aliases
alias ..='cd ..'
alias ld='exa -d'          # Show info about the directory
alias ll='exa -l'          # Show long file information
alias l='exa'          # Show long file information
alias la='exa -la'          # Show hidden files
alias lt='exa -L=2 -T'         # Sort by date, most recent last

# The ubiquitous 'll': directories first, with alphanumeric sorting:
#alias ll='ls -lv --group-directories-first'

alias cp="cp -i"
alias mv="mv -i"

alias du='du -h'
alias job='jobs -l'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# load ENV.sh
source ./dotfiles/ENV.sh

# Use if colordiff exists
if hash 'colordiff'; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

alias vi="nvim"
alias vim="nvim"

#vim
export XDG_CONFIG_HOME="$HOME/.config"

# peco + ghq
alias gcd='ghq look `ghq list |fzf --preview "bat --color=always --style=header,grid --line-range :80 $(ghq root)/{}/README.*"`'
function ghql() {
  local selected_file=$(ghq list --full-path | fzf --query "$LBUFFER")
  if [ -n "$selected_file" ]; then
    if [ -t 1 ]; then
      echo ${selected_file}
      cd ${selected_file}
      pwd
    fi
  fi
}
bind -x '"\C-g": ghql'

function fzf-checkout-branch() {
  git branch -a | fzf | xargs git checkout
}
bind -x '"\C-b": fzf-checkout-branch'

# prompt
# Setting bash-completion
source /usr/local/etc/bash_completion.d/git-prompt.sh
source /usr/local/etc/bash_completion.d/git-completion.bash

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

GIT_PS1_SHOWDIRTYSTATE=true
# GIT_PS1_SHOWUPSTREAM=true
# GIT_PS1_SHOWUNTRACKEDFILES=true
# GIT_PS1_SHOWSTASHSTATE=true

function _emoji() {
    echo -e '\U1F363'
}

# export PS1='\[\e[0;34m\]\W\[\e[m\] on \[\e[0;35m\]$(__git_ps1)\[\e[m\]\$ '
export PS1='\[\e[0;34m\]\W\[\e[m\] on\[\e[0;35m\]$(__git_ps1)\[\e[m\] \$ '
# export PS1='\[\e[37;100m\] \# \[\e[90;47m\]\[\e[30;47m\] \W \[\e[37m\]$(__git_ps1 "\[\e[37;102m\] \[\e[30m\] %s \[\e[0;92m\]")\[\e[49m\]\[\e[m\] \$ '

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

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

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
