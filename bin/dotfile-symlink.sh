#!/bin/bash

# Dotfilesのベースディレクトリを指定
dotfiles_dir=~/dotfiles/config

# ベースディレクトリ内の全てのディレクトリを取得
for tool_dir in "$dotfiles_dir"/*; do
  if [ -d "$tool_dir" ]; then
    # ツールディレクトリのベース名を取得
    tool_name=$(basename "$tool_dir")

    # 対応するディレクトリを定義
    target_dir="$HOME/.config/$tool_name"

    # 既存のディレクトリがシンボリックリンクの場合はunlink
    if [ -L "$target_dir" ]; then
      unlink "$target_dir"
      echo "既存のシンボリックリンクをunlinkしました: $target_dir"
    elif [ -e "$target_dir" ]; then
      # 通常のディレクトリの場合はバックアップ
      mv "$target_dir" "$target_dir.backup"
      echo "既存のディレクトリをバックアップしました: $target_dir -> $target_dir.backup"
    fi

    # シンボリックリンクを作成
    ln -s "$tool_dir" "$target_dir"
    echo "シンボリックリンクを作成しました: $target_dir -> $tool_dir"
  fi
done
