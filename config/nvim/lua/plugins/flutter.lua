return {
  -- Flutter tools
  {
    "akinsho/flutter-tools.nvim",
    lazy = false,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim",
    },
    ft = { "dart" },
    opts = {
      closing_tags = {
        enabled = false,
      },
    },
  },
}
