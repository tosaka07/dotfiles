return {
  {
    "mfussenegger/nvim-dap",
    cmd = { "Dap" },
    config = function()
      local keymap = vim.keymap.set
      local dap = require('dap')

      vim.fn.sign_define("DapBreakpoint", { text = "🔴", texthl = "", linehl = "", numhl = "" })
      vim.fn.sign_define("DapBreakpointCondition", { text = "⚠️", texthl = "", linehl = "", numhl = "" })
      vim.fn.sign_define("DapBreakpointRejected", { text = "❌", texthl = "", linehl = "", numhl = "" })
      vim.fn.sign_define("DapLogPoint", { text = "💬", texthl = "", linehl = "", numhl = "" })
      vim.fn.sign_define("DapStopped", { text = "▶️", texthl = "", linehl = "", numhl = "" })

      local opts = { noremap = true, silent = true }
      keymap('n', '<leader>bt', function() dap.toggle_breakpoint() end, opts)
      keymap('n', '<leader>bc', function() dap.continue() end, opts)
      keymap('n', '<leader>bi', function() dap.step_into() end, opts)
      keymap('n', '<leader>bo', function() dap.step_over() end, opts)
      keymap('n', '<leader>br', function() dap.clear_breakpoints() end, opts)
    end
  },
  {
    "rcarriga/nvim-dap-ui",
    cmd = { "Dap" },
    dependencies = {
      "mfussenegger/nvim-dap",
    },
    config = function()
      local dapui = require("dapui")

      dapui.setup()

      local opts = { noremap = true, silent = true }
      vim.keymap.set('n', '<leader>bu', function() dapui.toggle() end, opts)
    end
  },
}
