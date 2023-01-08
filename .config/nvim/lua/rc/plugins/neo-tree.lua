return {
  "nvim-neo-tree/neo-tree.nvim",
  cmd = "Neotree",
  config = function()
    -- vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

    local sign_define = vim.fn.sign_define
    sign_define("DiagnosticSignError", {text = " ", texthl = "DiagnosticSignError"})
    sign_define("DiagnosticSignWarn", {text = " ", texthl = "DiagnosticSignWarn"})
    sign_define("DiagnosticSignInfo", {text = " ", texthl = "DiagnosticSignInfo"})
    sign_define("DiagnosticSignHint", {text = "", texthl = "DiagnosticSignHint"})

    require("neo-tree").setup {
      popup_border_style = "rounded",
      enable_git_status = true,
      enable_diagnostics = true,
      filesystem = {
        bind_to_cwd = true,
        cwd_target = {
          sidebar = "tab", -- sidebar is when position = left or right
          current = "window" -- current is when position = current
        },
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = false,
        },
        follow_current_file = true,
        hijack_netrw_behavior = "open_current",
      },
      window = {
        width = 36,
      },
    }
  end
}
