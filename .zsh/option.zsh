
# ヒストリーサイズ設定
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# 自動補完
# autoload -Uz compoinit && compinit

# 単語の入力中でもTab補完を有効化
setopt complete_in_word

# 補完の選択を楽にする
zstyle ':completion:*' menu select

# 補完候補を詰めて表示する
setopt list_packed

# 補完候補に色を付ける
autoload -U colors ; colors ; zstyle ':completion:*' list-colors "${LS_COLORS}"

# 大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 補完候補をTabや矢印で移動できる
zstyle ":completion:*:default" menu select=1

# Tabで補完候補を切り替える
setopt auto_menu

# 補完候補を一覧表示
setopt auto_list

# 変数名を補完する
setopt auto_param_keys

# 保管時にヒストリを自動的に展開する
setopt hist_expand

# ディレクトリ名を補完すると、末尾がスラッシュになる。
setopt auto_param_slash

# BEEPを鳴らさない
setopt no_beep

# Make
zstyle ':completion:*:*:make:*' tag-order 'targets'
