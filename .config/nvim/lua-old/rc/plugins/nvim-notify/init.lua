local status_ok, notify = pcall(require, "notify")
if not status_ok then
  vim.notify("Failed to load notify", "error")
end

vim.notify = notify
notify.setup {
  background_colour = "#000000",
  fps = 60,
}
