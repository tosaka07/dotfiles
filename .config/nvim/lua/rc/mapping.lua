---@diagnostic disable: missing-parameter

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
local opts = { noremap = true, silent = true }

-- Escape
keymap("i", "jj", "<ESC>", opts)
keymap('i', '<C-g>', '<ESC>', opts)
keymap('n', '<C-g>', '<ESC>', opts)
keymap('n', '<Esc><Esc>', ':nohlsearch<CR>', opts)

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

-- Which Keys
local leader = {
  --["w"] = {
  --  name = "+window",
  --},
  x = {
    name = "+file",
    s = { ":w<CR>", "save" },
    c = { ":q<CR>", "quit" },
  },
  d = {
    name = "+dial",
    i = { 
      function()
        -- TODO: lua から呼べばいいがうまく動かないから調査
        require("dial.map")
        vim.api.nvim_command(":DialIncrement")
      end,
      "increment" 
    },
    d = { 
      function()
        require("dial.map")
        vim.api.nvim_command(":DialDecrement")
      end,
      "increment" 
    },
  },
  e = {
    name = "+window",
    K = { ":resize -4<CR>", "resize-horizontal-4" },
    J = { ":resize +4<CR>", "resize-horizontal+4" },
    H = { ":vertical resize -4<CR>", "resize-vertical-4" },
    L = { ":vertical resize +4<CR>", "resize-vertical+4" },
    h = { "<C-w>h", "move-left" },
    k = { "<C-w>k", "move-up" },
    j = { "<C-w>j", "move-down" },
    l = { "<C-w>l", "move-right" },
    e = { ":Neotree<CR>", "neotree-open" },
    s = { ":vsplit<CR>", "split-vertical" },
    S = { ":split<CR>", "split-horizontal" },
  },
  f = {
    name = "+find",
    f = { ":Telescope current_buffer_fuzzy_find fuzzy=false case_mode=ignore_case<CR>", "current-buffer-find" },
    p = { ":Telescope find_files<CR>", "find-files" },
    h = { ":Telescope frecency<CR>", "history-files" },
    g = { ":Telescope live_grep<CR>", "live-grep" },
    b = { ":Telescope buffers<CR>", "buffers" },
    e = { ":Telescope file_browser<CR>", "file-browser" },
  }
}

local wk = require("which-key")
wk.register(leader, { prefix = "<leader>" })
wk.setup({
  plugins = {
    presets = {
      operators = false,
    }
  }
})
