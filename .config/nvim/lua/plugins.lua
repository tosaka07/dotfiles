-- Manage plugins using packer

local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function()
  use({ "wbthomason/packer.nvim", opt = true })

  -- Improve startup time for Neovim
	use 'lewis6991/impatient.nvim'

  -- Base
  use({ "nvim-lua/popup.nvim", module = "popup" })
	use({ "nvim-lua/plenary.nvim" })
	use({ "kkharji/sqlite.lua", module = "sqlite" })
	use({ "MunifTanjim/nui.nvim", module = "nui" })


  -- Theme
  local colorscheme = "nightfox.nvim"
  use { 
    'EdenEast/nightfox.nvim',
    run = ":NightfoxCompile",
    event = { "VimEnter", "ColorSchemePre" },
    config = function()
      require('nightfox').setup({
        options = {
          transparent = true,
        }
      })
      vim.cmd([[ colorscheme duskfox ]])
    end
  }

  -- Statusline
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true },
    after = { colorscheme },
    config = function()
      require('lualine').setup()
    end
  }

  -- Comment
  use {
    'numToStr/Comment.nvim',
    event = "VimEnter",
    config = function()
      require('Comment').setup {
        pre_hook = function(ctx)
          local U = require 'Comment.utils'

          local location = nil
          if ctx.ctype == U.ctype.block then
            location = require('ts_context_commentstring.utils').get_cursor_location()
          elseif ctx.cmotion == U.cmotion.v or ctx.cmotion == U.cmotion.V then
            location = require('ts_context_commentstring.utils').get_visual_start_location()
          end

          return require('ts_context_commentstring.internal').calculate_commentstring {
            key = ctx.ctype == U.ctype.line and '__default' or '__multiline',
            location = location,
          }
        end,
      }
    end
  }
  
  -- Treesitter
  use {
    'nvim-treesitter/nvim-treesitter',
    after = { colorscheme },
    run = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = "all",
        ignore_install = { "phpdoc" },
        highlight = {
          enable = true,
        },
        context_commentstring = {
          enable = true,
          enable_autocmd = false,
        },
        tree_docs = {
          enable = true,
        },
        rainbow = {
		      enable = true,
		      extended_mode = true,
		      -- max_file_lines = 300,
	      },
        yati = {
          enable = true,
        },
      })
    end,
  }
  use { "JoosepAlviste/nvim-ts-context-commentstring", after = { "nvim-treesitter" } }
	use { "nvim-treesitter/nvim-tree-docs", after = { "nvim-treesitter" } }
  use { "p00f/nvim-ts-rainbow", after = { "nvim-treesitter" } }
	use { "yioneko/nvim-yati", after = "nvim-treesitter" }

  -- Auto completion
  use {
		"hrsh7th/nvim-cmp",
		requires = {
      { "L3MON4D3/LuaSnip", opt = true, event = "VimEnter" },
			{ "windwp/nvim-autopairs", opt = true, event = "VimEnter" },
		},
		after = { "LuaSnip", "nvim-autopairs" },
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        snippet = {
          expand = function(args)
            require('luasnip').lsp_expand(args.body)
          end
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }),
        }),
        sources = {
          { name = "buffer" },
          { name = "path" },
          { name = "nvim_lua" },
          { name = "emoji" },
          { name = "calc" },
          { name = "spell" },
          { name = "luasnip" },
          { name = 'treesitter' },
          { name = 'rg' },
        },
        sorting = {
          comparators = {
            cmp.config.compare.offset,
            cmp.config.compare.exact,
            cmp.config.compare.score,
            require("cmp-under-comparator").under,
            cmp.config.compare.kind,
            cmp.config.compare.sort_text,
            cmp.config.compare.length,
            cmp.config.compare.order,
          },
        },
      })

      -- cmp.setup.cmdline(':', {
      --   mapping = cmp.mapping.present.cmdline(),
      --   sources = {
      --     { name = "cmdline" }
      --   }
      -- })
      -- cmp.setup.cmdline('/', {
      --   sources = {
      --     { name = "buffer" }
      --   }
      -- })
    end
	}
  --  use({
	-- 	"onsails/lspkind-nvim",
	-- 	module = "lspkind",
	-- })
	-- use({ "hrsh7th/cmp-nvim-lsp", module = "cmp_nvim_lsp" })
	-- use({ "hrsh7th/cmp-nvim-lsp-signature-help", after = "nvim-cmp" })
	-- use({ "hrsh7th/cmp-nvim-lsp-document-symbol", after = "nvim-cmp" })
	
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
  
  -- LSP
  use "neovim/nvim-lspconfig"
  use { 
    "williamboman/nvim-lsp-installer",
    config = function()
      require('nvim-lsp-installer').setup()
    end
  }
  use {
    "folke/trouble.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true },
    config = function()
      require("trouble").setup()
    end
  }
  use {
    'j-hui/fidget.nvim',
    config = function()
      require('fidget').setup()
    end
  }
  -- use 'folke/lsp-colors.nvim'
  
  if packer_bootstrap then
    require('packer').sync()
  end
end)
