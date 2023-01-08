local status_ok, format = pcall(require, "lsp-format")
if not status_ok then
  vim.notify("Failed to load lsp-format", "error")
end

format.setup()
