# Documentation: https://github.com/romkatv/zsh4humans/blob/v5/README.md.
#
# Do not modify this file unless you know exactly what you are doing.
# It is strongly recommended to keep all shell customization and configuration
# (including exported environment variables such as PATH) in ~/.zshrc or in
# files sourced from ~/.zshrc. If you are certain that you must export some
# environment variables in ~/.zshenv, do it where indicated by comments below.

if [ -n "${ZSH_VERSION-}" ]; then
    # If you are certain that you must export some environment variables
    # in ~/.zshenv (see comments at the top!), do it here:
    #
    #   export GOPATH=$HOME/go
    #
    # Do not change anything else in this file.

    # XDG
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_CACHE_HOME="$HOME/.cache"
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_STATE_HOME="$HOME/.local/state"

    # envrc
    if [ -f ~/dotfiles/.envrc ]; then
        source "$HOME/dotfiles/.envrc"
    fi

    # ASDF
    export ASDF_CONFIG_FILE="$XDG_CONFIG_HOME/asdf/asdfrc"
    export ASDF_DATA_DIR="$XDG_DATA_HOME/asdf"

    # Android
    export ANDROID_HOME="$XDG_DATA_HOME"/android
    export GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle

    # lesshist
    export LESSHISTFILE="$XDG_CACHE_HOME"/less/history

    # gnupghome
    export GNUPGHOME="$XDG_DATA_HOME"/gnupg

    # terminfo
    export TERMINFO="$XDG_DATA_HOME"/terminfo
    export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo

    # node
    export NODE_REPL_HISTORY="$XDG_DATA_HOME"/node_repl_history
    export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc

    # python
    export PYTHONSTARTUP="${XDG_CONFIG_HOME}/python/pythonrc"

    # zsh
    export HISTFILE="$XDG_STATE_HOME"/zsh/history


    : ${ZDOTDIR:=~}
    setopt no_global_rcs
    [[ -o no_interactive && -z "${Z4H_BOOTSTRAPPING-}" ]] && return
    setopt no_rcs
    unset Z4H_BOOTSTRAPPING
fi

Z4H_URL="https://raw.githubusercontent.com/romkatv/zsh4humans/v5"
: "${Z4H:=${XDG_CACHE_HOME:-$HOME/.cache}/zsh4humans/v5}"

umask o-w

if [ ! -e "$Z4H"/z4h.zsh ]; then
    mkdir -p -- "$Z4H" || return
    >&2 printf '\033[33mz4h\033[0m: fetching \033[4mz4h.zsh\033[0m\n'
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL -- "$Z4H_URL"/z4h.zsh >"$Z4H"/z4h.zsh.$$ || return
    elif command -v wget >/dev/null 2>&1; then
        wget -O-   -- "$Z4H_URL"/z4h.zsh >"$Z4H"/z4h.zsh.$$ || return
    else
        >&2 printf '\033[33mz4h\033[0m: please install \033[32mcurl\033[0m or \033[32mwget\033[0m\n'
        return 1
    fi
    mv -- "$Z4H"/z4h.zsh.$$ "$Z4H"/z4h.zsh || return
fi

. "$Z4H"/z4h.zsh || return

setopt rcs
