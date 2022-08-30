local mason_lspconfig_status_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
if not mason_lspconfig_status_ok then
  vim.notify("Failed to load mason-lspconfig", "error")
end

local on_attach = function(client, bufnr)
  -- plugins
  require('lsp-format').on_attach(client)
  require("nvim-navic").attach(client, bufnr)

  -- manual keymaps
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  -- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  -- vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', 'gtD', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  -- vim.keymap.set('n', 'gn', vim.lsp.buf.rename, bufopts)
  -- vim.keymap.set('n', 'ge', vim.diagnostic.open_float, bufopts)
  -- vim.keymap.set('n', 'g[', vim.diagnostic.goto_next, bufopts)
  -- vim.keymap.set('n', 'g]', vim.diagnostic.goto_prev, bufopts)
  -- vim.keymap.set('n', 'ga', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gf', vim.lsp.buf.formatting, bufopts)

  vim.cmd [[
    augroup lsp_document_highlight
      autocmd! * <buffer>
      autocmd CursorHold,CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
      autocmd CursorMoved,CursorMovedI <buffer> lua vim.lsp.buf.clear_references()
    augroup END
  ]]
end

local lspconfig = require("lspconfig")
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)
local opts = { capabilities = capabilities, on_attach = on_attach }

mason_lspconfig.setup_handlers {
  function(server_name)
    lspconfig[server_name].setup(opts)
  end
}
