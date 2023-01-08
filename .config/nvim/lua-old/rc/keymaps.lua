-- COMMANDS                    MODES ~
-- :map   :noremap  :unmap     Normal, Visual, Select, Operator-pending
-- :nmap  :nnoremap :nunmap    Normal
-- :vmap  :vnoremap :vunmap    Visual and Select
-- :smap  :snoremap :sunmap    Select
-- :xmap  :xnoremap :xunmap    Visual
-- :omap  :onoremap :ounmap    Operator-pending
-- :map!  :noremap! :unmap!    Insert and Command-line
-- :imap  :inoremap :iunmap    Insert>
-- :lmap  :lnoremap :lunmap    Insert, Command-line, Lang-Arg
-- :cmap  :cnoremap :cunmap    Command-line
-- :tmap  :tnoremap :tunmap    Terminal

-- -------------------------------------------------------------------------------------------|
--  Modes      | Normal | Insert | Command | Visual | Select | Operator | Terminal | Lang-Arg |
--  [nore]map  |    @   |   -    |    -    |   @    |   @    |    @     |    -     |    -     |
--  n[nore]map |    @   |   -    |    -    |   -    |   -    |    -     |    -     |    -     |
--  n[orem]ap! |    -   |   @    |    @    |   -    |   -    |    -     |    -     |    -     |
--  i[nore]map |    -   |   @    |    -    |   -    |   -    |    -     |    -     |    -     |
--  c[nore]map |    -   |   -    |    @    |   -    |   -    |    -     |    -     |    -     |
--  v[nore]map |    -   |   -    |    -    |   @    |   @    |    -     |    -     |    -     |
--  x[nore]map |    -   |   -    |    -    |   @    |   -    |    -     |    -     |    -     |
--  s[nore]map |    -   |   -    |    -    |   -    |   @    |    -     |    -     |    -     |
--  o[nore]map |    -   |   -    |    -    |   -    |   -    |    @     |    -     |    -     |
--  t[nore]map |    -   |   -    |    -    |   -    |   -    |    -     |    @     |    -     |
--  l[nore]map |    -   |   @    |    @    |   -    |   -    |    -     |    -     |    @     |
-- -------------------------------------------------------------------------------------------"

-- i.e.
-- normal_mode = "n",
-- insert_mode = "i",
-- visual_mode = "v",
-- visual_block_mode = "x",
-- term_mode = "t",
-- command_mode = "c",


local keymap = vim.keymap.set
local global = vim.g
local opts = { noremap = true, silent = true }

-- Leader
keymap('', '<Space>', '<Nop>', opts)
global.mapleader = " "
global.maplocalleader = " "

-- Escape
keymap("i", "jj", "<ESC>", opts)
keymap('i', '<C-g>', '<ESC>', opts)
keymap('n', '<C-g>', '<ESC>', opts)
keymap('n', '<Esc><Esc>', ':nohlsearch<CR>', opts)

-- Save & Quit
keymap('n', '<leader>w', ':w<CR>', opts)
keymap('n', '<leader>q', ':q<CR>', opts)
keymap('n', '<Leader>x', ':bp <BAR> bd #<CR>', opts)

-- Move window
keymap('n', '<leader>h', '<C-w>h', opts)
keymap('n', '<leader>k', '<C-w>k', opts)
keymap('n', '<leader>j', '<C-w>j', opts)
keymap('n', '<leader>l', '<C-w>l', opts)

-- Resize window
keymap("n", "<leader>sj", ":resize -3<CR>", opts)
keymap("n", "<leader>sk", ":resize +3<CR>", opts)
keymap("n", "<leader>sl", ":vertical resize -3<CR>", opts)
keymap("n", "<leader>sh", ":vertical resize +3<CR>", opts)

-- Split window
keymap('n', '<leader>ss', ':split<Return><C-w>w', opts)
keymap('n', '<leader>sv', ':vsplit<Return><C-w>w', opts)

keymap('n', 'j', 'gj', opts)
keymap('n', 'k', 'gk', opts)

-- Emacs bindings
keymap('i', '<C-p>', '<Up>', opts)
keymap('i', '<C-n>', '<Down>', opts)
keymap('i', '<C-b>', '<Left>', opts)
keymap('i', '<C-f>', '<Right>', opts)
keymap('i', '<C-a>', '<Home>', opts)
keymap('i', '<C-e>', '<End>', opts)
keymap('i', '<C-d>', '<Del>', opts)
keymap('i', '<C-h>', '<BS>', opts)
keymap('i', '<C-k>', '<Esc><Right>C', opts)

keymap('n', '<C-p>', '<Up>', opts)
keymap('n', '<C-n>', '<Down>', opts)
keymap('n', '<C-b>', '<Left>', opts)
keymap('n', '<C-f>', '<Right>', opts)
keymap('n', '<C-a>', '<Home>', opts)
keymap('n', '<C-e>', '<End>', opts)
keymap('n', '<C-d>', '<Del>', opts)
keymap('n', '<C-h>', '<BS>', opts)
keymap('n', '<C-k>', 'D', opts)

-- NeoTree
keymap('n', '<leader>ee', '<cmd>NeoTreeReveal<CR>', opts)
keymap('n', '<leader>es', '<cmd>NeoTreeShowToggle<CR>', opts)
