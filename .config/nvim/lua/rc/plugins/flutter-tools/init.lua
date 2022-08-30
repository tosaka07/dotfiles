require("flutter-tools").setup {
  ui = {
    border = "rounded",
    notification_style = 'plugin'
  },
  decorations = {
    statusline = {
      app_version = false,
      device = true,
    }
  },
  outline = {
    auto_open = false,
  },
  debugger = {
    enabled = true,
    run_via_dap = true,
    register_configurations = function(paths)
      require("dap").configurations.dart = {}
      require("dap.ext.vscode").load_launchjs()
    end,
  },
  flutter_lookup_cmd = "asdf where flutter",
  widget_guides = {
    enabled = true,
  },
  closing_tags = {
    enabled = false,
  },
  lsp = {
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
    color = {
      enabled = true,
      background = true,
      foreground = false,
      virtual_text = true,
      virtual_text_str = "■",
    },
  },
}
