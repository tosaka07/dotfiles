local keymap = vim.keymap.set

local dap = require('dap')

vim.fn.sign_define("DapBreakpoint", { text = "🛑", texthl = "", linehl = "", numhl = "" })

local opts = { noremap = true, silent = true }
keymap('n', '<leader>b', function() dap.toggle_breakpoint() end, opts)
keymap('n', '<leader>bc', function() dap.continue() end, opts)
keymap('n', '<leader>bi', function() dap.step_into() end, opts)
keymap('n', '<leader>bo', function() dap.step_over() end, opts)
