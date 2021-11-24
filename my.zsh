# ---------------
# z4h
# ---------------

# ---------------
# Export
# ---------------

# ENV
source "$HOME/dotfiles/ENV.sh"

# Reset the paths
if [ -x /usr/libexec/path_helper ]; then
  unset PATH
  eval `/usr/libexec/path_helper -s`
fi

# Homebrew
ARCH=$(uname -m)
if [ "${ARCH}" = "x86_64" ]; then
  # Autoload /usr/local/bin from path_helper if mac
  if [ "$(sysctl -in sysctl.proc_translated)" = "1" ]; then
    echo "Running on Rosetta 2"
  else
    echo "Running on native Intel"
  fi
elif [ "${ARCH}" = "arm64" ]; then
  export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
  echo "Running on ARM64"
else
  echo "Unknown architecture: ${ARCH}"
fi

# Remove duplicates PATH
typeset -gU PATH

# ---------------
# Alias
# ---------------

if type "exa" > /dev/null 2>&1; then
  alias ld="exa -d"
  alias ll="exa -l"
  alias l="exa"
  alias la="exa -la"
  alias lt="exa -L=2 -T"
  alias lt3="exa -L=3 -T"	
fi

if type "nvim" > /dev/null 2>&1; then
  alias vi="nvim"
  alias vim="nvim"
fi

alias ishell="arch -x86_64 /bin/zsh"
alias ashell="arch -arm64 /bin/zsh"

# ---------------
# Option
# ---------------

# Make
zstyle ':completion:*:*:make:*' tag-order 'targets'

# ディレクトリ名を補完すると、末尾がスラッシュになる。
setopt auto_param_slash


# ---------------
# Function
# ---------------

# ghq + fzf
function ghq-fzf() {
  local src=$(ghq list | fzf --layout=reverse --preview "bat --color=always --style=header,grid --line-range :80 $(ghq root)/{}/(README|readme).*")
  if [ -n "$src" ]; then
    BUFFER="cd $(ghq root)/$src"
    zle accept-line
  fi
  zle -R -c
}
zle -N ghq-fzf
z4h bindkey ghq-fzf Ctrl+G

# branch + fzf
function branch-fzf() {
  local branch
  branch=$(git branch -a | tr -d " " |fzf --layout=reverse --height 100% --prompt "checkout branch>" --preview "git log --color=always {}" | head -n 1 | sed -e "s/^\*\s*//g" | perl -pe "s/remotes\/origin\///g")
  git checkout $(echo "$branch")
  zle accept-line
}
zle -N branch-fzf
z4h bindkey branch-fzf Ctrl+B

# delete xcode cache
function delete-xcode-cache() {
  rm -rf ~/Library/Developer/Xcode/DerivedData/*  
}
