local status_ok, saga = pcall(require, "lspsaga")
if not status_ok then
  vim.notify("Failed to load lspsaga", "error")
end

saga.init_lsp_saga()

local keymap = vim.keymap
local opts = { noremap = true, silent = true }

keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", opts)
keymap.set("n", "gh", "<cmd>Lspsaga lsp_finder<CR>", opts)
keymap.set("n", "ga", "<cmd>Lspsaga code_action<CR>", opts)
keymap.set("v", "ga", "<cmd><C-U>Lspsaga range_code_action<CR>", opts)
keymap.set("n", "gn", "<cmd>Lspsaga rename<CR>", opts)
keymap.set("n", "gd", "<cmd>Lspsaga preview_definition<CR>", opts)
keymap.set("n", "ge", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
keymap.set("n", "g[", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
keymap.set("n", "g]", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)
