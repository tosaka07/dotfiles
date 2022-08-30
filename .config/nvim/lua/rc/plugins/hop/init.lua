require 'hop'.setup { keys = 'asdfghjklurmvo' }
local api = vim.api
api.nvim_set_keymap('', 'f',
  "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>"
  , {})
api.nvim_set_keymap('', 'F',
  "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>"
  , {})
api.nvim_set_keymap('', 't',
  "<cmd>lua require'hop'.hint_words()<cr>"
  , {})
api.nvim_set_keymap('', 'T',
  "<cmd>lua require'hop'.hint_patterns()<cr>"
  , {})
