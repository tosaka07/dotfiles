local opt = vim.opt

-- Encoding
opt.encoding = "utf-8"
opt.fileencodings = "utf-8,sjis,iso-2022-jp,euc-jp"
opt.fileformats = "unix,mac,dos"

-- Number
opt.number = true
opt.relativenumber = false
opt.signcolumn = "yes"

-- Backup
opt.swapfile = false
opt.backup = false
opt.writebackup = false

-- File
opt.hidden = true
opt.autoread = true
opt.conceallevel = 0

-- Completions
opt.completeopt = { "menuone", "noselect" }

-- Indent
opt.smartindent = true
opt.expandtab = true
opt.tabstop = 2
opt.shiftwidth = 2
opt.softtabstop = 2

-- Seaching
opt.hlsearch = true

-- Mouse
opt.mouse = 'a'

opt.updatetime = 300

-- Split
opt.splitbelow = true
opt.splitright = true

-- highlights
opt.termguicolors = true
opt.cursorline = true
opt.termguicolors = true
opt.winblend = 0
opt.wildoptions = 'pum'
opt.pumblend = 5
opt.background = 'dark'
