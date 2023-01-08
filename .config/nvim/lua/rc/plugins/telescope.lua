local M = {
  "nvim-telescope/telescope.nvim",
  cmd = { "Telescope" },
  dependencies = {
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    { "nvim-telescope/telescope-frecency.nvim" },
    { "nvim-telescope/telescope-file-browser.nvim" },
  },
  config = function()
    local telescope = require("telescope")
    telescope.setup()
    telescope.load_extension("fzf")
    telescope.load_extension("frecency")
    telescope.load_extension("file_browser")
  end
}

return M
