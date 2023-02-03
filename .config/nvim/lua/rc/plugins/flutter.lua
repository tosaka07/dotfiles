return {
  "akinsho/flutter-tools.nvim",
  event = {
    "BufReadPre *.dart",
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  config = function()
    require("flutter-tools").setup {
      ui = {
        border = "rounded",
        notification_style = 'plugin'
      },
      decorations = {
        statusline = {
          app_version = true,
          device = true,
        }
      },
      outline = {
        auto_open = false,
      },
      debugger = {
        enabled = true,
        run_via_dap = true,
        register_configurations = function(paths)
          require("dap").configurations.dart = {}
          require("dap.ext.vscode").load_launchjs()
        end,
      },
      flutter_lookup_cmd = "asdf where flutter",
      widget_guides = {
        enabled = true,
      },
      closing_tags = {
        enabled = false,
      },
      dev_log = { enabled = false, open_cmd = 'tabedit' },
      lsp = {
        on_attach = function(client, bufnr)
          -- plugins
          require("lsp-format").on_attach(client)
          -- if client.server_capabilities.documentSymbolProvider then
          --   require("nvim-navic").attach(client, bufner)
          -- end

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

          -- code action
          vim.api.nvim_create_autocmd("BufWritePre", {
            pattern = { "*.dart" },
            callback = function()
              -- local params = vim.lsp.util.make_range_params()
              -- params.context = { only = { "source.organizeImports" } }
              --
              -- local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
              -- print(vim.inspect(result))
              -- for _, res in pairs(result or {}) do
              --   for _, r in pairs(res.result or {}) do
              --     if r.edit then
              --       vim.notify("edit")
              --       vim.lsp.util.apply_workspace_edit(r.edit, vim.lsp.util._get_offset_encoding())
              --     else
              --       vim.notify("not edit")
              --       vim.lsp.buf.execute_command(r.command)
              --     end
              --   end
              -- end

              local current_bufnr = vim.api.nvim_get_current_buf()

              vim.b.format_changedtick = vim.b.changedtick
              local params = vim.lsp.util.make_range_params()
              params.context = {
                diagnostics = {},
                only = { "source.fixAll" }
              }

              local hook = function(err, result, ctx)
                for _, r in pairs(result or {}) do
                  if r.edit then
                    vim.lsp.util.apply_workspace_edit(r.edit)
                  else
                    vim.lsp.buf.execute_command(r.command)
                  end
                end

                if err ~= nil then
                  local client = vim.lsp.get_client_by_id(ctx.client_id)
                  local client_name = client and client.name or string.format("client_id=%d", ctx.client_id)
                  vim.notify(string.format("%s: %d: %s", client_name, err.code, err.message), "error")
                  return
                end
                if result == nil then
                  return
                end
                if not vim.api.nvim_buf_is_loaded(ctx.bufnr) then
                  vim.fn.bufload(ctx.bufnr)
                  vim.api.nvim_buf_set_var(ctx.bufnr, "format_changedtick",
                    vim.api.nvim_buf_get_var(ctx.bufnr, "changedtick"))
                end
                if vim.api.nvim_buf_get_var(ctx.bufnr, "format_changedtick")
                    ~= vim.api.nvim_buf_get_var(ctx.bufnr, "changedtick")
                    or vim.startswith(vim.api.nvim_get_mode().mode, "i") then
                  return
                end

                -- vim.lsp.util.apply_text_edits(result, ctx.bufnr, "utf-16")
                if ctx.bufnr == vim.api.nvim_get_current_buf() then
                  -- FIXME: 同期処理実装
                  vim.cmd [[sleep 100m]]
                  vim.cmd [[update]]
                end
              end

              client.request("textDocument/codeAction", params, hook, current_bufnr)

              -- local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 2000)
              -- vim.lsp.buf_request(0, "textDocument/codeAction", params, function(err, result, ctx)
              --
              --   for _, r in pairs(result or {}) do
              --     if r.edit then
              --       vim.lsp.util.apply_workspace_edit(r.edit)
              --     else
              --       vim.lsp.buf.execute_command(r.command)
              --     end
              --
              --
              --     print(vim.inspect(result))
              --     vim.lsp.util.apply_text_edits({ r }, current_bufnr, "utf-16")
              --     vim.cmd [[update]]
              --   end
              --
              --   if not vim.api.nvim_buf_is_loaded(current_bufnr) then
              --     vim.fn.bufload(current_bufnr)
              --   end
              --
              -- end)

              --
              -- if result.result == nil then
              --   return
              -- end
              --
              -- if not vim.api.nvim_buf_is_loaded(current_bufnr) then
              --   vim.fn.bufload(current_bufnr)
              -- end
              -- vim.lsp.util.apply_text_edits(result.result, current_bufnr, "utf-16")
              -- vim.cmd [[update]]
              --
            end
          })
        end,
        capabilities = require('cmp_nvim_lsp').default_capabilities(),
        color = {
          enabled = true,
          background = true,
          foreground = false,
          virtual_text = true,
          virtual_text_str = "■",
        },
      },
    }
  end
}
