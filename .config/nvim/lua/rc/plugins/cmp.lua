return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "lukas-reineke/cmp-under-comparator",
    "onsails/lspkind.nvim",
    "L3MON4D3/LuaSnip",
    'saadparwaiz1/cmp_luasnip',
  },
  config = function()
    vim.o.completeopt = "menuone,noselect"
    local cmp = require("cmp")
    local cmp_autopairs = require('nvim-autopairs.completion.cmp')

    cmp.event:on(
      'confirm_done',
      cmp_autopairs.on_confirm_done()
    )

    cmp.setup {
      snippet = {
        expand = function(args)
          require('luasnip').lsp_expand(args.body)
        end
      },
      mapping = cmp.mapping.preset.insert {
        ["<C-p>"] = cmp.mapping.select_prev_item(),
        ["<C-n>"] = cmp.mapping.select_next_item(),
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
      },
      sources = {
        { name = "nvim_lsp" },
        {
          name = "buffer",
          options = {
            get_bufners = function()
              local buf = vim.api.nvim_get_current_buf()
              local byte_size = vim.api.nvim_buf_get_offset(buf, vim.api.nvim_buf_line_count(buf))
              if byte_size > 1024 * 1024 then -- 1 Megabyte max
                return {}
              end
              return { buf }
            end
          }
        },
        { name = "luasnip" },
        { name = "path" },
        { name = "treesitter" },
        { name = "nvim_lsp_signature_help" },
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
      formatting = {
        format = require("lspkind").cmp_format {
          mode = 'symbol_text',
          maxwidth = 50,
          before = function(entry, vim_item)
            return vim_item
          end
        }
      },
      experimental = {
        ghost_text = true,
      },
    }
  end
}
