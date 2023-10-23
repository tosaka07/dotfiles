-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

function Map(mode, lhs, rhs, opts)
  local keys = require("lazy.core.handler").handlers.keys
  ---@cast keys LazyKeysHandler
  -- do not create the map if a lazy keys handler exists
  if not keys.active[keys.parse({ lhs, mode = mode }).id] then
    opts = opts or {}
    opts.silent = opts.silent ~= false
    vim.keymap.set(mode, lhs, rhs, opts)
  end
end

local opts = { noremap = true, silent = true }

-- Better up/down
Map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
Map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- Escape
Map("i", "jj", "<ESC>", opts)
Map({ "i", "n", "v" }, "<C-g>", "<ESC>", opts)
Map("n", "<Esc><Esc>", "<cmd>nohlsearch<CR>", opts)
Map("n", "<C-g><C-g>", function()
  vim.cmd("nohlsearch")
  vim.fn.setreg("/", "")
end, opts)

-- Emacs bindings
Map("i", "<C-p>", "<Up>", opts)
Map("i", "<C-n>", "<Down>", opts)
Map("i", "<C-b>", "<Left>", opts)
Map("i", "<C-f>", "<Right>", opts)
Map("i", "<C-a>", "<Home>", opts)
Map("i", "<C-e>", "<End>", opts)
Map("i", "<C-d>", "<Del>", opts)
Map("i", "<C-h>", "<BS>", opts)
Map("i", "<C-k>", "<Esc><Right>C", opts)

Map("n", "<C-p>", "<Up>", opts)
Map("n", "<C-n>", "<Down>", opts)
Map("n", "<C-b>", "<Left>", opts)
Map("n", "<C-f>", "<Right>", opts)
Map("n", "<C-a>", "<Home>", opts)
Map("n", "<C-e>", "<End>", opts)
Map("n", "<C-d>", "<Del>", opts)
Map("n", "<C-h>", "<BS>", opts)
Map("n", "<C-k>", "D", opts)

-- Better indenting
Map("v", "<", "<gv")
Map("v", ">", ">gv")
