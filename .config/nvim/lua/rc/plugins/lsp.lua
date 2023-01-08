return {
  "williamboman/mason-lspconfig.nvim",
  "neovim/nvim-lspconfig",
  {
    "williamboman/mason.nvim",
    event = { "BufReadPost" },
    config = function()
      local mason = require("mason")
      local mason_lspconfig = require("mason-lspconfig")
      local lspconfig = require("lspconfig")
      -- Don't change order
      mason.setup()
      mason_lspconfig.setup({
        automatic_installation = true,
        ensure_installed = {
          "bashls",
          "cssls",
          "cssmodules_ls",
          "eslint",
          "html",
          "jsonls",
          "kotlin_language_server",
          "sumneko_lua",
          "ruby_ls",
          "rust_analyzer",
          "tailwindcss",
          "tsserver",
          "yamlls",
        },
      })

      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      local on_attach = function(client, bufner)
        -- plugins
        require("lsp-format").on_attach(client)
        if client.server_capabilities.documentSymbolProvider then
          require("nvim-navic").attach(client, bufner)
        end

        -- manual keymaps
        local bufopts = { noremap = true, silent = true, buffer = bufner }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
        -- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
        -- vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
        vim.keymap.set('n', 'gtD', vim.lsp.buf.type_definition, bufopts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
        vim.keymap.set('n', 'gf', function() vim.lsp.buf.formatting { async = true } end, bufopts)
        vim.cmd("xnoremap <silent><buffer> <Leader>f :lua vim.lsp.buf.range_formatting({})<CR>")
        -- vim.keymap.set('n', 'gn', vim.lsp.buf.rename, bufopts)
        -- vim.keymap.set('n', 'ge', vim.diagnostic.open_float, bufopts)
        -- vim.keymap.set('n', 'g[', vim.diagnostic.goto_next, bufopts)
        -- vim.keymap.set('n', 'g]', vim.diagnostic.goto_prev, bufopts)
        -- vim.keymap.set('n', 'ga', vim.lsp.buf.code_action, bufopts)

        if client.server_capabilities.document_highlight then
          vim.cmd [[
            augroup lsp_document_highlight
              autocmd! * <buffer>
              autocmd CursorHold,CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
              autocmd CursorMoved,CursorMovedI <buffer> lua vim.lsp.buf.clear_references()
            augroup END
          ]]
        end
      end

      mason_lspconfig.setup_handlers {
        function(server_name)
          lspconfig[server_name].setup {
            capabilities = capabilities,
            on_attach = on_attach,
          }
        end,
        ['sumneko_lua'] = function()
          lspconfig.sumneko_lua.setup {
            capabilities = capabilities,
            on_attach = on_attach,
            settings = {
              Lua = {
                diagnostics = {
                  globals = { 'vim' },
                }
              }
            }
          }
        end
      }
    end
  },
  {
    "kkharji/lspsaga.nvim",
    event = { "BufReadPre" },
    config = function()
      require("lspsaga").setup()

      local keymap = vim.keymap
      local opts = { noremap = true, silent = true }

      keymap.set("n", "K", "<cmd>Lspsaga hover_doc<CR>", opts)
      keymap.set("n", "gh", "<cmd>Lspsaga lsp_finder<CR>", opts)
      keymap.set("n", "ga", "<cmd>Lspsaga code_action<CR>", opts)
      keymap.set("v", "ga", "<cmd><C-U>Lspsaga range_code_action<CR>", opts)
      keymap.set("n", "gn", "<cmd>Lspsaga rename<CR>", opts)
      keymap.set("n", "gd", "<cmd>Lspsaga preview_definition<CR>", opts)
      keymap.set("n", "ge", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
      keymap.set("n", "g[", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
    end
  },
  {
    "lukas-reineke/lsp-format.nvim",
    config = function()
      require("lsp-format").setup()
    end
  },
  {
    "SmiteshP/nvim-navic",
    config = function()
      require("nvim-navic").setup()
    end
  },
  {
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup()
    end
  }
}
