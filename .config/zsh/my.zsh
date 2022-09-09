# *Warning*
# This shell assumes that you are using z4h.
# see: https://github.com/romkatv/zsh4humans

# ---------------
# Export
# ---------------

# ENV
# Reset the paths
if [ -x /usr/libexec/path_helper ]; then
  unset PATH
  eval `/usr/libexec/path_helper -s`
fi

# Homebrew
source ${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/opt/asdf/libexec/asdf.sh}
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"

# Dart
if type "dart" > /dev/null 2>&1; then
    export PATH="$HOME/.pub-cache/bin:$PATH"
fi

# Flutter
if type "flutter" > /dev/null 2>&1; then
    export FLUTTER_ROOT=$(asdf where flutter)
fi

# Android
if [ -d "$HOME/Library/Android/sdk/platform-tools" ]; then
    export PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
fi

compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-"$ZSH_VERSION"

# git-delta
if ! type "delta" > /dev/null 2>&1; then
    echo "delta is not installed, please install it for git diffs with the following command."
    echo "  brew install git-delta"
fi

# hammerspoon
# defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"

# Remove duplicates PATH
typeset -gU PATH

if type "exa" > /dev/null 2>&1; then
  alias ld="exa -d"
  alias ll="exa -l"
  alias l="exa"
  alias la="exa -la"
  alias lt="exa -L=2 -T"
  alias lt3="exa -L=3 -T"
else
  echo "exa is not installed, please install it for customized ls with the following command."
  echo "  brew install exa"
fi

if type "nvim" > /dev/null 2>&1; then
  alias vi="nvim"
  alias vim="nvim"
fi

if [ -e /Applications/Neovide.app ]; then
  alias neovide='open /Applications/Neovide.app --env NEOVIDE_MULTIGRID'
fi

if type "emacs" > /dev/null 2>&1; then
  alias e="emacs -nw"
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
# z4h bindkey branch-fzf Ctrl+B

# delete xcode cache
function delete-xcode-cache() {
  rm -rf ~/Library/Developer/Xcode/DerivedData/*
}

function zsh-startuptime() {
  local total_msec=0
  local msec
  local i
  for i in $(seq 1 10); do
    msec=$((TIMEFMT='%mE'; time zsh -i -c exit) 2>/dev/stdout >/dev/null)
    msec=$(echo $msec | tr -d "ms")
    echo "${(l:2:)i}: ${msec} [ms]"
    total_msec=$(( $total_msec + $msec ))
  done
  local average_msec
  average_msec=$(( ${total_msec} / 10 ))
  echo "\naverage: ${average_msec} [ms]"
}

function zsh-startuptime-slower-than-default() {
  local time_rc
  time_rc=$((TIMEFMT="%mE"; time zsh -i -c exit) &> /dev/stdout)
  # time_norc=$((TIMEFMT="%mE"; time zsh -df -i -c exit) &> /dev/stdout)
  # compinit is slow
  local time_norc
  time_norc=$((TIMEFMT="%mE"; time zsh -df -i -c "autoload -Uz compinit && compinit -C; exit") &> /dev/stdout)
  echo "my zshrc: ${time_rc}\ndefault zsh: ${time_norc}\n"

  local result
  result=$(scale=3 echo "${time_rc%ms} / ${time_norc%ms}" | bc)
  echo "${result}x slower your zsh than the default."
}

function nvim-startuptime() {
  local file=$1
  local total_msec=0
  local msec
  local i
  for i in $(seq 1 10); do
    msec=$({(TIMEFMT='%mE'; time nvim --headless -c q $file ) 2>&3;} 3>/dev/stdout >/dev/null)
    msec=$(echo $msec | tr -d "ms")
    echo "${(l:2:)i}: ${msec} [ms]"
    total_msec=$(( $total_msec + $msec ))
  done
  local average_msec
  average_msec=$(( ${total_msec} / 10 ))
  echo "\naverage: ${average_msec} [ms]"
}

function nvim-startuptime-slower-than-default() {
  local file=$1
  local time_file_rc
  time_file_rc=$(mktemp --suffix "_nvim_startuptime_rc.txt")
  local time_rc
  time_rc=$(nvim --headless --startuptime ${time_file_rc} -c "quit" $file > /dev/null && tail -n 1 ${time_file_rc} | cut -d " " -f1)

  local time_file_norc
  time_file_norc=$(mktemp --suffix "_nvim_startuptime_norc.txt")
  local time_norc
  time_norc=$(nvim --headless --noplugin -u NONE --startuptime ${time_file_norc} -c "quit" $file > /dev/null && tail -n 1 ${time_file_norc} | cut -d " " -f1)

  echo "my vimrc: ${time_rc}s\ndefault neovim: ${time_norc}s\n"
  local result
  result=$(scale=3 echo "${time_rc} / ${time_norc}" | bc)
  echo "${result}x slower your Neovim than the default."
}

function nvim-profiler() {
  local file=$1
  local time_file
  time_file=$(mktemp --suffix "_nvim_startuptime.txt")
  echo "output: $time_file"
  time nvim --headless --startuptime $time_file -c q $file
  tail -n 1 $time_file | cut -d " " -f1 | tr -d "\n" && echo " [ms]\n"
  cat $time_file | sort -n -k 2 | tail -n 20
}


function create-gif() {
  function _error() {
    echo -e "\e[31m $1 \e[m"
  }

  if type "ffmpeg" > /dev/null 2>&1; then

    echo "Start creating GIF..."

    local palette_path="./palette.png"
    local input=$1
    local output=$2
    local fps=$3

    echo "INPUT: $input"
    echo "OUTPUT: $output"
    echo "FPS: $fps"

    ffmpeg -i $input -vf "palettegen" -y $palette_path
    ffmpeg -i $input -i $palette_path \
      -lavfi "fps=$fps,scale=300:-1:flags=lanczos [x]; [x][1:v] paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle" \
      -y $output
    rm $palette_path
  else
    _error "ffmpeg not found."
  fi
}

function archive-screenshots() {
  local saved_dir=$HOME/Pictures/Screenshot
  local archive_dir="$HOME/Pictures/Screenshot_archive/$(date '+%Y-%m-%d')"
  mkdir -p $archive_dir
  mv $HOME/Pictures/Screenshot/* $archive_dir
  echo "Archived the screenshots to $archive_dir"
}
