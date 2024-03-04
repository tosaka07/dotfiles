# Delete start message
set fish_greeting

eval (/opt/homebrew/bin/brew shellenv)


. /opt/homebrew/share/google-cloud-sdk/path.fish.inc


# ----------------------------------------------
# Path
# ----------------------------------------------
fish_add_path $HOME/.pub-cache/bin
fish_add_path $HOME/.cargo/bin
fish_add_path $HOME/.rye/bin
fish_add_path $HOME/Library/Android/sdk/platform-tools
fish_add_path $HOME/.istioctl/bin

# ----------------------------------------------
# Aliases
# ----------------------------------------------
alias ls="eza"
alias ll="eza -lF --time-style=long-iso"
alias la="eza -laF --time-style=long-iso"
alias lt="eza -T"
alias lta="eza -T -a"
alias tree="eza -TF"

alias vi="nvim"
alias vim="nvim"

if type -q trash-put
    alias rm='trash-put'
end

if type -q mise
    mise activate fish | source
    set -gx FLUTTER_ROOT "$(mise where flutter)"
    alias asdf="mise"
end

if type -q gitui
    alias gitui="gitui -t mocha.ron"
end


# ----------------------------------------------
# Variables
# ----------------------------------------------
set -g FZF_TMUX 1
set -g FZF_TMUX_OPTS -p


# ----------------------------------------------
# Plugin: fzf
# ----------------------------------------------
set -Ux FZF_DEFAULT_OPTS "\
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"


# ----------------------------------------------
# Plugin: zoxide
# ----------------------------------------------
if type -q zoxide
    zoxide init fish | source
    alias c="z"
end

# ----------------------------------------------
# Functions
# ----------------------------------------------

function ghq_cd_fzf -d "Change dirctory to selected local repo managed by ghq."
    set -l input (commandline)
    set src (ghq list | fzf-tmux -p 80% -q "$input" --layout=reverse --preview "glow --style dark --width 80 "(ghq root)"/{}/README.md")
    if test -n "$src"
        cd (ghq root)/"$src"
        commandline -f repaint
    end
end
bind \cg ghq_cd_fzf

function history_fzf -d "Fuzzy search history"
    set -l input (commandline)
    set cmd (history | fzf-tmux -p 80% -q "$input" --layout reverse)
    if test -n "$cmd"
        commandline -r -- "$cmd"
    end
end
bind \cr history_fzf

function zoxide_fzf -d "Change directory to selected directory managed by zoxide"
    set -l input (commandline)
    set src (zoxide query --list | fzf-tmux -p -q "$input" --layout=reverse --preview='ls {} --color always --icons' --preview-window=down,30%,sharp)
    if test -n "$src"
        cd $src
        commandline -f repaint
    end
end
alias cdd=zoxide_fzf

function prevd_without_newline
    prevd >/dev/null
    commandline -f repaint
end
bind \e\[1\;2D prevd_without_newline

function nextd_without_newline
    nextd >/dev/null
    commandline -f repaint
end
bind \e\[1\;2C nextd_without_newline

function cd_parent_without_newline
    cd ..
    commandline -f repaint
end
bind \e\[1\;2A cd_parent_without_newline

function cd_child_without_newline
    # Get all directories
    set child_dirs (exa -d */)

    # Check the number of directories
    if count $child_dirs >1
        # If more than one directory, use fzf to select
        set selected_dir (printf "%s\n" $child_dirs | fzf --layout=reverse --preview 'exa -l --time-style=long-iso {}')
    else if count $child_dirs = 1
        # If only one directory, select it directly
        set selected_dir $child_dirs
    end

    # If a directory was selected, move to it
    if test -n "$selected_dir"
        cd $selected_dir
    end

    commandline -f repaint
end
bind \e\[1\;2B cd_child_without_newline

function clia
    read -l line
    commandline -a $line
    # commandline -a 
end

bind -M insert \et fuzzy_complete

function fuzzy_complete
    complete -C | sort -u | fzf --height 40% --multi --reverse -q (commandline -t) | cut --output-delimiter ' ' -f1 | sed s/-//g | clia
    commandline -f end-of-line
end
