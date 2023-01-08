-- Manage plugins using packer

local packer = nil
local function init()
  if packer == nil then
    packer = require "packer"
    packer.init {
      disable_commands = true,
      open_fn = require("packer.util").float,
    }
  end

  local use = packer.use
  packer.reset()

  -- Packer
  use { "wbthomason/packer.nvim", opt = true }

  -- Improve launch speed
  use "lewis6991/impatient.nvim"

  -- Base
  --  TODO: require で定義してこれは消す
  use { "nvim-lua/popup.nvim", module = "popup" }
  use { "nvim-lua/plenary.nvim" }
  use { "kkharji/sqlite.lua", module = "sqlite" }
  use { "MunifTanjim/nui.nvim", module = "nui" }
  use {
    "rcarriga/nvim-notify",
    config = function()
      require("rc.plugins.nvim-notify")
    end
  }

  -- Theme
  local colorscheme = "nightfox.nvim"
  use {
    "edenEast/nightfox.nvim",
    run = ":NightfoxCompile",
    opt = true,
    event = { "VimEnter", "ColorSchemePre" },
    config = function()
      require("rc.plugins.nightfox")
    end
  }

  -- Statusline
  use {
    "nvim-lualine/lualine.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true },
    after = { colorscheme },
    config = function()
      require("rc.plugins.lualine")
    end
  }

  -- git
  use {
    'lewis6991/gitsigns.nvim',
    opt = true,
    event = { 'BufRead', 'BufNewFile' },
    config = function()
      require("rc.plugins.gitsigns")
    end
  }

  -- Treesitter
  use { "JoosepAlviste/nvim-ts-context-commentstring", after = "nvim-treesitter" }
  use { "nvim-treesitter/nvim-tree-docs", after = "nvim-treesitter" }
  use { "p00f/nvim-ts-rainbow", after = "nvim-treesitter" }
  use { "yioneko/nvim-yati", after = "nvim-treesitter" }
  use { "nvim-treesitter/nvim-treesitter-context", after = "nvim-treesitter" }
  use { "windwp/nvim-ts-autotag", after = "nvim-treesitter" }
  use { "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" }
  use {
    'nvim-treesitter/nvim-treesitter',
    after = { colorscheme },
    run = ":TSUpdate",
    config = function()
      require("rc.plugins.nvim-treesitter")
    end,
  }

  -- Comment
  use {
    'numToStr/Comment.nvim',
    opt = true,
    event = "VimEnter",
    config = function()
      require("rc.plugins.comment")
    end
  }

  -- Auto completion
  use {
    "onsails/lspkind-nvim",
    module = "lspkind",
    after = "nvim-cmp"
  }
  use { "hrsh7th/cmp-nvim-lsp", module = "cmp_nvim_lsp" }
  use { "hrsh7th/cmp-nvim-lsp-signature-help", after = "nvim-cmp" }
  use { "hrsh7th/cmp-buffer", after = "nvim-cmp" }
  use { "hrsh7th/cmp-path", after = "nvim-cmp" }
  use { "hrsh7th/cmp-nvim-lua", after = "nvim-cmp" }
  use { "hrsh7th/cmp-emoji", after = "nvim-cmp" }
  use { "hrsh7th/cmp-calc", after = "nvim-cmp" }
  use { "hrsh7th/cmp-cmdline", after = "nvim-cmp" }
  use { "f3fora/cmp-spell", after = "nvim-cmp" }
  use { "saadparwaiz1/cmp_luasnip", after = "nvim-cmp" }
  use { "ray-x/cmp-treesitter", after = "nvim-cmp" }
  use { "lukas-reineke/cmp-rg", after = "nvim-cmp" }
  use { "lukas-reineke/cmp-under-comparator", module = "cmp-under-comparator" }
  use {
    "hrsh7th/nvim-cmp",
    requires = {
      { "L3MON4D3/LuaSnip", opt = true, event = "VimEnter" },
      {
        "windwp/nvim-autopairs",
        opt = true,
        event = "VimEnter",
        config = function()
          require("rc.plugins.nvim-autopairs")
        end
      },
    },
    after = { "LuaSnip", "nvim-autopairs" },
    config = function()
      require("rc.plugins.nvim-cmp")
    end
  }

  -- LSP
  use {
    "williamboman/mason.nvim",
    -- event = "VimEnter",
    config = function()
      require("rc.plugins.mason")
    end
  }
  use {
    "lukas-reineke/lsp-format.nvim",
    -- event = "VimEnter",
    config = function()
      require("rc.plugins.lsp-format")
    end
  }
  use {
    "SmiteshP/nvim-navic",
    -- event = "VimEnter",
    config = function()
      require("rc.plugins.nvim-navic")
    end
  }
  use {
    "neovim/nvim-lspconfig",
    -- event = "VimEnter",
    config = function()
      require("rc.plugins.nvim-lspconfig")
    end
  }
  use {
    "williamboman/mason-lspconfig.nvim",
    -- after = {
    --   "mason.nvim",
    --   "nvim-lspconfig",
    --   "cmp-nvim-lsp",
    --   "lsp-format.nvim",
    --   "nvim-navic"
    -- },
    config = function()
      require("rc.plugins.mason-lspconfig")
    end
  }
  use {
    "glepnir/lspsaga.nvim",
    config = function()
      require("rc.plugins.lspsaga")
    end
  }
  use {
    'jose-elias-alvarez/null-ls.nvim',
    config = function()
      require("rc.plugins.null-ls")
    end
  }
  use {
    "folke/trouble.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true },
    config = function()
      require("rc.plugins.trouble")
    end
  }
  use {
    'j-hui/fidget.nvim',
    config = function()
      require("rc.plugins.fidget")
    end
  }

  -- Language
  use {
    'MunifTanjim/prettier.nvim',
    config = function()
      require("rc.plugins.prettier")
    end
  }
  use {
    'akinsho/flutter-tools.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function()
      require("rc.plugins.flutter-tools")
    end
  }

  -- DAP
  use {
    'mfussenegger/nvim-dap',
    event = "VimEnter",
    config = function()
      require("rc.plugins.nvim-dap")
    end
  }
  use {
    'rcarriga/nvim-dap-ui',
    after = "nvim-dap",
    config = function()
      require("rc.plugins.nvim-dap-ui")
    end
  }

  -- Telescope
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      { 'nvim-lua/plenary.nvim', opt = true },
      { 'nvim-lua/popup.nvim', opt = true },
    },
    after = { colorscheme },
    config = function()
      require("rc.plugins.telescope")
    end
  }
  use {
    "nvim-telescope/telescope-frecency.nvim",
    after = "telescope.nvim",
    config = function()
      require("telescope").load_extension("frecency")
    end
  }
  use {
    "nvim-telescope/telescope-file-browser.nvim",
    after = "telescope.nvim",
    config = function()
      require("telescope").load_extension("file_browser")
    end
  }
  use {
    "nvim-telescope/telescope-ghq.nvim",
    after = "telescope.nvim",
    config = function()
      require("telescope").load_extension("ghq")
    end
  }

  -- Project
  use {
    "ahmedkhalf/project.nvim",
    after = "telescope.nvim",
    config = function()
      require("rc.plugins.project")
    end
  }

  -- NeoTree
  use {
    "nvim-neo-tree/neo-tree.nvim",
    event = "VimEnter",
    requires = {
      { "nvim-lua/plenary.nvim" },
      { "kyazdani42/nvim-web-devicons" },
      { "MunifTanjim/nui.nvim" },
    },
    config = function()
      require("rc.plugins.neo-tree")
    end
  }

  -- bufferline
  use {
    'akinsho/bufferline.nvim',
    event = "VimEnter",
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require("rc.plugins.bufferline")
    end
  }

  -- Key Helper
  use {
    "folke/which-key.nvim",
    event = "VimEnter",
    config = function()
      require("rc.plugins.which-key")
    end
  }

  -- cursor line
  use {
    'mvllow/modes.nvim',
    event = "VimEnter",
    config = function()
      require("rc.plugins.modes")
    end
  }

  -- Color visualize
  use {
    'norcalli/nvim-colorizer.lua',
    event = "VimEnter",
    config = function()
      require("rc.plugins.colorizer")
    end
  }

  -- Move
  use {
    'phaazon/hop.nvim',
    branch = 'v2',
    event = "VimEnter",
    config = function()
      require("rc.plugins.hop")
    end
  }

  -- Surrounding delimiter pairs
  use {
    "kylechui/nvim-surround",
    event = "VimEnter",
    config = function()
      require("rc.plugins.nvim-surround")
    end
  }

end

local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end,
})

return plugins
