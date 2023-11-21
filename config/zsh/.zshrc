# ███████╗ ██╗  ██╗ ██╗  ██╗
# ╚══███╔╝ ██║  ██║ ██║  ██║
#   ███╔╝  ███████║ ███████║
#  ███╔╝   ╚════██║ ██╔══██║
# ███████╗      ██║ ██║  ██║
# ╚══════╝      ╚═╝ ╚═╝  ╚═╝
# Documentation: https://github.com/romkatv/zsh4humans/blob/v5/README.md

# Periodic auto-update on Zsh startup: 'ask' or 'no'.
# You can manually run `z4h update` to update everything.
zstyle ':z4h:' auto-update      'no'
# Ask whether to auto-update this often; has no effect if auto-update is 'no'.
zstyle ':z4h:' auto-update-days '28'

# Keyboard type: 'mac' or 'pc'.
zstyle ':z4h:bindkey' keyboard  'mac'

# Don't start tmux.
zstyle ':z4h:' start-tmux       no

# Mark up shell's output with semantic information.
zstyle ':z4h:' term-shell-integration 'yes'

# Right-arrow key accepts one character ('partial-accept') from
# command autosuggestions or the whole thing ('accept')?
zstyle ':z4h:autosuggestions' forward-char 'accept'

# Recursively traverse directories when TAB-completing files.
zstyle ':z4h:fzf-complete' recurse-dirs 'no'

# Enable direnv to automatically source .envrc files.
zstyle ':z4h:direnv'         enable 'yes'
# Show "loading" and "unloading" notifications from direnv.
zstyle ':z4h:direnv:success' notify 'yes'

# Enable ('yes') or disable ('no') automatic teleportation of z4h over
# SSH when connecting to these hosts.
zstyle ':z4h:ssh:example-hostname1'   enable 'yes'
zstyle ':z4h:ssh:*.example-hostname2' enable 'no'
# The default value if none of the overrides above match the hostname.
zstyle ':z4h:ssh:*'                   enable 'no'

# Send these files over to the remote host when connecting over SSH to the
# enabled hosts.
zstyle ':z4h:ssh:*' send-extra-files '~/.nanorc' '~/.env.zsh'

# Clone additional Git repositories from GitHub.
#
# This doesn't do anything apart from cloning the repository and keeping it
# up-to-date. Cloned files can be used after `z4h init`. This is just an
# example. If you don't plan to use Oh My Zsh, delete this line.
z4h install ohmyzsh/ohmyzsh || return

# Install or update core components (fzf, zsh-autosuggestions, etc.) and
# initialize Zsh. After this point console I/O is unavailable until Zsh
# is fully initialized. Everything that requires user interaction or can
# perform network I/O must be done above. Everything else is best done below.
z4h init || return

# ----------------------------------------------
# Path
# ----------------------------------------------

# Extend PATH.
# (N/): ディレクトリが存在するときだけ path に追加する
# (N-/): symlink を含むディレクトリが存在するときだけ path に追加する
path=(
  ~/bin(N-/)
  # Homebrew
  $HOMEBREW_PREFIX/bin(N-/)
  $HOMEBREW_PREFIX/sbin(N-/)
  # Dart
  $HOME/.pub-cache/bin(N-/)
  # Rust
  $HOME/.cargo/bin(N-/)
  # rye
  $HOME/.rye/shims(N-/)
  # Android
  $HOME/Library/Android/sdk/platform-tools(N-/)
  # istioctl 
  # TODO: brew に以降したいが 1.15.3 固定
  $HOME/.istioctl/bin(N-/)
  # base
  $path
)

# Remove duplicates PATH
typeset -gU PATH

# ----------------------------------------------
# Environment variables
# ----------------------------------------------
export GPG_TTY=$TTY


# ----------------------------------------------
# Plugins
# ----------------------------------------------
z4h load ohmyzsh/ohmyzsh/plugins/z

# ----------------------------------------------
# ASDF
# ----------------------------------------------
. /opt/homebrew/opt/asdf/libexec/asdf.sh


# ----------------------------------------------
# google-cloud-sdk
# ----------------------------------------------
. /opt/homebrew/share/google-cloud-sdk/path.zsh.inc

# ----------------------------------------------
# Key bindings
# ----------------------------------------------
z4h bindkey undo Ctrl+/   Shift+Tab  # undo the last command line change
z4h bindkey redo Option+/            # redo the last undone command line change

z4h bindkey z4h-cd-back    Shift+Left   # cd into the previous directory
z4h bindkey z4h-cd-forward Shift+Right  # cd into the next directory
z4h bindkey z4h-cd-up      Shift+Up     # cd into the parent directory
z4h bindkey z4h-cd-down    Shift+Down   # cd into a child directory

# Autoload functions.
autoload -Uz zmv

# Define functions and completions.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

# Define named directories: ~w <=> Windows home directory on WSL.
[[ -z $z4h_win_home ]] || hash -d w=$z4h_win_home


# ----------------------------------------------
# Aliases
# ----------------------------------------------
alias ls="eza --icons"
alias ll="eza -lF --time-style=long-iso --icons"
alias la="eza -lFa --time-style=long-iso --icons"
alias lt="eza -T"
alias lta="eza -T -a"
alias tree="eza -FT --icons"

alias vi="nvim"
alias vim="nvim"

# ----------------------------------------------
# Options
# ----------------------------------------------
# Set shell options: http://zsh.sourceforge.net/Doc/Release/Options.html.
setopt glob_dots     # no special treatment for file names with a leading dot
setopt no_auto_menu  # require an extra TAB press to open the completion menu

# Make
zstyle ':completion:*:*:make:*' tag-order 'targets'

# ディレクトリ名を補完すると、末尾がスラッシュになる。
setopt auto_param_slash

# git
zstyle ':completion:*:*:git:*' tag-order '(
  sorted
  local-files
  remote-branches
  tags
  detached-HEAD
  heads
  file-history
  cached
  command-line
)'
zstyle ':completion:*:*:git:*' file-sort name


# ----------------------------------------------
# Funtions
# ----------------------------------------------
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

function asdf-plugin-install() {
  # .tool-versions ファイルの存在を確認
  if [ ! -f ".tool-versions" ]; then
      echo ".tool-versions ファイルが見つかりません。"
      exit 1
  fi
  awk '{print $1}' .tool-versions | xargs -I {} asdf plugin add {}
}
