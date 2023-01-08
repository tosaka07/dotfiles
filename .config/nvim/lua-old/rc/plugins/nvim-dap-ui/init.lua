local dapui = require("dapui")

dapui.setup()

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<leader>bu', function() dapui.toggle() end, opts)
