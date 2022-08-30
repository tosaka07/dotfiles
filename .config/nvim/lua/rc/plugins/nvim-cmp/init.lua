local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
  vim.notify("Failed to load cmp", "error")
end

local autopairs_cmp_status_ok, cmp_autopairs = pcall(require, "nvim-autopairs.completion.cmp")
if not autopairs_cmp_status_ok then
  vim.notify("Failed to load cmp_autopairs", "error")
end

local lspkind_status_ok, lspkind = pcall(require, "lspkind")
if not lspkind_status_ok then
  vim.notify("Failed to load lspkind", "error")
end

local luasnip_status_ok, luasnip = pcall(require, "luasnip")
if not luasnip_status_ok then
  vim.notify("Failed to load luasnip", "error")
end

local comparator_status_ok, comparator = pcall(require, "cmp-under-comparator")
if not comparator_status_ok then
  vim.notify("Failed to load cmp-under-comparator")
end

cmp.event:on(
  'confirm_done',
  cmp_autopairs.on_confirm_done()
)

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
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
    { name = "path" },
    {
      name = "buffer",
      option = {
        get_bufnrs = function()
          local buf = vim.api.nvim_get_current_buf()
          local byte_size = vim.api.nvim_buf_get_offset(buf, vim.api.nvim_buf_line_count(buf))
          if byte_size > 1024 * 1024 then -- 1 Megabyte max
            return {}
          end
          return { buf }
        end
      }
    },
    { name = "nvim_lua" },
    { name = "emoki" },
    { name = "calc" },
    { name = "spell" },
    { name = "luasnip" },
    { name = 'treesitter' },
    { name = 'rg' },
    { name = 'nvim_lsp_signature_help' },
  },
  sorting = {
    comparators = {
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      cmp.config.compare.score,
      comparator.under,
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },
  formatting = {
    format = lspkind.cmp_format {
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
