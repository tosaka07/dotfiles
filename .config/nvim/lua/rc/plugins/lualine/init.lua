local status_ok, lualine = pcall(require, "lualine")
if not status_ok then
  vim.notify("Failed to load lualine", "error")
end

lualine.setup {
  options = {
    icons_enabled = true,
  }
}
