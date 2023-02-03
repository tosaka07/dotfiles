return {
  {
    "TimUntersberger/neogit",
    cmd = { "Neogit" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      'sindrets/diffview.nvim',
    },
    config = function()
      local neogit = require('neogit')
      neogit.setup {
        integrations = {
          diffview = true
        }
      }
    end
  },
  {
    'akinsho/git-conflict.nvim',
    event = "BufReadPre",
  }
}
