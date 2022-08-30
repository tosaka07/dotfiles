#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title NeoVide
# @raycast.mode silent
# @raycast.packageName Developer Utilities
#
# Optional parameters:
# @raycast.icon ⚒️
# @raycast.needsConfirmation false
#
# Documentation:
# @raycast.description Open Neivide
# @raycast.author tosaka07
# @raycast.authorURL https://github.com/tosaka07

if [ -e /Applications/Neovide.app ]; then
  open /Applications/Neovide.app --env NEOVIDE_MULTIGRID
fi

