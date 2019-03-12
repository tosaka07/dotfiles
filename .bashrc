# Common aliases
alias ..='cd ..'
alias ld='ls -ldG'          # Show info about the directory
alias ll='ls -lFG'          # Show long file information
alias l='ls -1FG'          # Show long file information
alias la='ls -lAFG'          # Show hidden files
alias lx='ls -lXBG'         # Sort by extension
alias lk='ls -lSrG'         # Sort by size, biggest last
alias lc='ls -ltcrG'        # Sort by and show change time, most recent last
alias lu='ls -lturG'        # Sort by and show access time, most recent last
alias lt='ls -ltrG'         # Sort by date, most recent last
alias lr='ls -lRG'          # Recursive ls

# The ubiquitous 'll': directories first, with alphanumeric sorting:
#alias ll='ls -lv --group-directories-first'

alias cp="cp -i"
alias mv="mv -i"

alias du='du -h'
alias job='jobs -l'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Alias

# Use if colordiff exists
if hash 'colordiff'; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

alias vi="nvim"
alias vim="nvim"

# peco + ghq
function ghql() {
  local selected_file=$(ghq list --full-path | peco --query "$LBUFFER")
  if [ -n "$selected_file" ]; then
    if [ -t 1 ]; then
      echo ${selected_file}
      cd ${selected_file}
      pwd
    fi
  fi
}

bind -x '"\201": ghql'
# Bind to C-g 
bind '"\C-g":"\201\C-m"'

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
