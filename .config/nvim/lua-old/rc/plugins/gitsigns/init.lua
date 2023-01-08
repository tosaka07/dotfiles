local status_ok, gitsigns = pcall(require, "gitsigns")
if not status_ok then
  vim.notify("Failed to load gitsigns", "error")
end

gitsigns.setup()
