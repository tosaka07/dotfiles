local status_ok, null_ls = pcall(require, "null-ls")
if not status_ok then
  vim.notify("Failed to load null-ls", "error")
end

null_ls.setup {
  sources = {
    null_ls.builtins.formatting.prettier.with {
      prettier_local = "node_modules/.bin"
    }
  },
  on_attach = function(client, bufnr)
  end,
}
