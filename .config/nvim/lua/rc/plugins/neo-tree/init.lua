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
  },
  window = {
    width = 36,
  },
}
