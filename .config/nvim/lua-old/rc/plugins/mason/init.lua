local status_ok, mason = pcall(require, "mason")
if not status_ok then
  vim.notify("Failed to load mason", "error")
end

mason.setup()
