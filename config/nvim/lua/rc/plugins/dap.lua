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
      keymap('n', '<leader>dt', function() dap.toggle_breakpoint() end, opts)
      keymap('n', '<leader>dc', function() dap.continue() end, opts)
      keymap('n', '<leader>di', function() dap.step_into() end, opts)
      keymap('n', '<leader>do', function() dap.step_over() end, opts)
      keymap('n', '<leader>dr', function() dap.clear_breakpoints() end, opts)
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
      vim.keymap.set('n', '<leader>du', function() dapui.toggle() end, opts)
    end
  },
}
