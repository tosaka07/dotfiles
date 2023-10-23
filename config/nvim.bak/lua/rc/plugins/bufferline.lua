return {
  "akinsho/bufferline.nvim",
  version = "*",
  dependencies = 'nvim-tree/nvim-web-devicons',
  event = "VeryLazy",
  opts = {
    options = {
      always_show_bufferline = false,
      offsets = {
        {
          filetype = "neo-tree",
          text = "Neo-tree",
          highlight = "Directory",
          text_align = "left",
        },
      },
    },
  },
  config = function()
    require("bufferline").setup()
  end
}
