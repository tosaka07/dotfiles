#!/bin/bash

for f in .??*
do
    [[ "$f" == ".git" ]] && continue
    [[ "$f" == ".DS_Store" ]] && continue

    echo "$f"
    unlink ~/$f
done

unlink ~/.config/alacritty/alacritty.yml
