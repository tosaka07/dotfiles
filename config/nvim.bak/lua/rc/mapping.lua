---@diagnostic disable<cmd> missing-parameter

-- COMMANDS                    MODES ~
-- <cmd>map   <cmd>noremap  <cmd>unmap     Normal, Visual, Select, Operator-pending
-- <cmd>nmap  <cmd>nnoremap <cmd>nunmap    Normal
-- <cmd>vmap  <cmd>vnoremap <cmd>vunmap    Visual and Select
-- <cmd>smap  <cmd>snoremap <cmd>sunmap    Select
-- <cmd>xmap  <cmd>xnoremap <cmd>xunmap    Visual
-- <cmd>omap  <cmd>onoremap <cmd>ounmap    Operator-pending
-- <cmd>map!  <cmd>noremap! <cmd>unmap!    Insert and Command-line
-- <cmd>imap  <cmd>inoremap <cmd>iunmap    Insert>
-- <cmd>lmap  <cmd>lnoremap <cmd>lunmap    Insert, Command-line, Lang-Arg
-- <cmd>cmap  <cmd>cnoremap <cmd>cunmap    Command-line
-- <cmd>tmap  <cmd>tnoremap <cmd>tunmap    Terminal

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

local map = require("rc.util").map

local opts = { noremap = true, silent = true }


-- Better up/down
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- Save file
map({ "i", "v", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save file" })

-- Escape
map("i", "jj", "<ESC>", opts)
map({ "i", "n", "v" }, '<C-g>', '<ESC>', opts)
map('n', '<Esc><Esc>', '<cmd>nohlsearch<CR>', opts)
map('n', '<C-g><C-g>', function()
  vim.cmd('nohlsearch')
  vim.fn.setreg('/', '')
end, opts)

-- Emacs bindings
map('i', '<C-p>', '<Up>', opts)
map('i', '<C-n>', '<Down>', opts)
map('i', '<C-b>', '<Left>', opts)
map('i', '<C-f>', '<Right>', opts)
map('i', '<C-a>', '<Home>', opts)
map('i', '<C-e>', '<End>', opts)
map('i', '<C-d>', '<Del>', opts)
map('i', '<C-h>', '<BS>', opts)
map('i', '<C-k>', '<Esc><Right>C', opts)

map('n', '<C-p>', '<Up>', opts)
map('n', '<C-n>', '<Down>', opts)
map('n', '<C-b>', '<Left>', opts)
map('n', '<C-f>', '<Right>', opts)
map('n', '<C-a>', '<Home>', opts)
map('n', '<C-e>', '<End>', opts)
map('n', '<C-d>', '<Del>', opts)
map('n', '<C-h>', '<BS>', opts)
map('n', '<C-k>', 'D', opts)

-- Better indenting
map("v", "<", "<gv")
map("v", ">", ">gv")


-- Which Keys
local leader = {
  ["0"] = { "<cmd>Lazy<CR>", "Lazy" },
  x = {
    name = "+file",
    s = { "<cmd>w<CR>", "Save" },
    c = { "<cmd>q<CR>", "Quit" },
  },
  w = {
    name = "+window",
    K = { "<cmd>resize -4<CR>", "resize-horizontal-4" },
    J = { "<cmd>resize +4<CR>", "resize-horizontal+4" },
    H = { "<cmd>vertical resize -4<CR>", "resize-vertical-4" },
    L = { "<cmd>vertical resize +4<CR>", "resize-vertical+4" },
    h = { "<C-w>h", "move-left" },
    k = { "<C-w>k", "move-up" },
    j = { "<C-w>j", "move-down" },
    l = { "<C-w>l", "move-right" },
    w = { "<cmd>Neotree<CR>", "neotree-open" },
    s = { "<cmd>vsplit<CR>", "split-vertical" },
    S = { "<cmd>split<CR>", "split-horizontal" },
  },
  f = { "<cmd>Telescope find_files<CR>", "Find files" },
  F = { "<cmd>Telescope live_grep<CR>", "Find in files (projects)" },
  b = { "<cmd>Telescope buffers<CR>", "Select buffer" },
  -- e = { "<cmd>Telescope file_browser<CR>", "file-browser" },
  g = {
    name = "+git",
    g = { function() require("gitui").open() end, "GitUI" }
  },
  l = {
    name = "+lsp",
    d = { "<cmd>Telescope diagnostics<CR>", "Diagnostics" },
    r = { "<cmd>Telescope lsp_references<CR>", "References" },
    t = { "<cmd>Telescope lsp_type_definitions<CR>", "Type definitions" },
    i = { "<cmd>Telescope lsp_implementations<CR>", "Implementations" },
    I = { "<cmd>Telescope lsp_incoming_calls<CR>", "Incoming calls" },
    I = { "<cmd>Telescope lsp_outgoing_calls<CR>", "Outgoing calls" },
  },
  ["/"] = { "<cmd>Telescope current_buffer_fuzzy_find fuzzy=false case_mode=ignore_case<CR>", "Search in a file" },
}

local wk = require("which-key")
wk.register(leader, { prefix = "<leader>" })
wk.setup({
  plugins = {
    presets = {
      operators = true,
    }
  }
})
