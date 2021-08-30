# --------------------------
# Export
# --------------------------

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
    eval "$(/usr/local/bin/brew shellenv)"
    # Autoload /usr/local/bin from path_helper if mac
    if [ "$(sysctl -in sysctl.proc_translated)" = "1" ]; then
        echo "Running on Rosetta 2"
    else
        echo "Running on native Intel"
    fi 
elif [ "${ARCH}" = "arm64" ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
    echo "Running on ARM"
else
    echo "Unknown architecture: ${ARCH}"
fi

# Starship via Homebrew
export STARSHIP_CONFIG="$HOME/dotfiles/starship.toml"
eval "$(starship init zsh)"

# rbenv
export PATH="$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH"

# fzf
[[ $- == *i* ]] && source "$HOME/.fzf/shell/completion.zsh" 2> /dev/null
source "$HOME/.fzf/shell/key-bindings.zsh"

# fnm
if type "fnm" > /dev/null 2>&1; then
  eval "$(fnm env)"
fi

# Google
#if [[ $ARCH == arm64 ]]; then
# source /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
# source /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
#fi

# Remvoe duplicates PATH
typeset -gU PATH
