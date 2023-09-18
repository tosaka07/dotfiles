local global = require("rc.global")

local function load_ons()
  local global_local = {
    -- Encoding
    encoding = "utf-8",
    fileencodings = "utf-8,sjis,iso-2022-jp,euc-jp",
    fileformats = "unix,mac,dos",
    -- Number
    number = true,
    relativenumber = false,
    signcolumn = "yes",
    -- Backup
    swapfile = false,
    backup = false,
    writebackup = false,
    undodir = global.cache_dir .. "undo/",
    undofile = true,
    -- File
    hidden = true,
    autoread = true,
    conceallevel = 0,
    history = 2000,
    -- Indent
    smartindent = true,
    expandtab = true,
    tabstop = 2,
    shiftwidth = 2,
    softtabstop = 2,
    -- Wrap
    wrap = false,
    -- Seaching
    hlsearch = true,
    -- Mouse
    mouse = 'a',
    updatetime = 300,
    -- Split
    splitbelow = true,
    splitright = true,
    -- highlights
    termguicolors = true,
    cursor = true,
    cursorline = true,
    winblend = 0,
    wildons = 'pum',
    pumblend = 5,
    background = 'dark',
    -- Command
    cmdheight = 0,
    -- Status line
    laststatus = 3, -- Use global status line
    -- bell
    errorbells = false,
    visualbell = false,
    -- clipboard
    clipboard = "unnamedplus",
    -- cursor wrap
    whichwrap = "b,s,h,l,<,>,[,],~",
    -- timeout(whichkey open timing)
    timeout = true,
    timeoutlen = 300,
  }

  local function isempty(s)
    return s == nil or s == ""
  end

  -- custom python provider
  local conda_prefix = os.getenv("CONDA_PREFIX")
  if not isempty(conda_prefix) then
    vim.g.python_host_prog = conda_prefix .. "/bin/python"
    vim.g.python3_host_prog = conda_prefix .. "/bin/python"
  elseif global.is_mac then
    vim.g.python_host_prog = "/usr/bin/python"
    vim.g.python3_host_prog = "/usr/local/bin/python3"
  else
    vim.g.python_host_prog = "/usr/bin/python"
    vim.g.python3_host_prog = "/usr/bin/python3"
  end

  for name, value in pairs(global_local) do
    vim.o[name] = value
  end
end

load_ons()
