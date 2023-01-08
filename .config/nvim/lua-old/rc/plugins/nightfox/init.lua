local status_ok, nightfox = pcall(require, "nightfox")
if not status_ok then
  vim.notify("Failed to load nightfox", "error")
end

nightfox.setup {
  options = {
    transparent = true,
  }
}

vim.cmd [[ colorscheme duskfox ]]
