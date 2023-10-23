return {
  {
    "aspeddro/gitui.nvim",
    config = function()
      local gitui = require('gitui')
      gitui.setup()
    end
  },
  {
    'akinsho/git-conflict.nvim',
    event = "BufReadPre",
  }
}
