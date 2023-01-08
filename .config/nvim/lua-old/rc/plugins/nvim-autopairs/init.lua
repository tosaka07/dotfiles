local status_ok, autopairs = pcall(require, "nvim-autopairs")
if not status_ok then
  vim.notify("Failed to load autopairs", "error")
end

autopairs.setup {
  check_ts = true,
}
