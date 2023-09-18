return {
	{
		"adelarsq/image_preview.nvim",
		event = "VeryLazy",
		config = function()
			require("image_preview").setup()
		end,
	},
	{
		"nvim-neo-tree/neo-tree.nvim",
		cmd = "Neotree",
		config = function()
			-- vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

			local sign_define = vim.fn.sign_define
			sign_define("DiagnosticSignError", { text = " ", texthl = "DiagnosticSignError" })
			sign_define("DiagnosticSignWarn", { text = " ", texthl = "DiagnosticSignWarn" })
			sign_define("DiagnosticSignInfo", { text = " ", texthl = "DiagnosticSignInfo" })
			sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint" })

			require("neo-tree").setup({
				popup_border_style = "rounded",
				enable_git_status = true,
				enable_diagnostics = true,
				filesystem = {
					bind_to_cwd = true,
					cwd_target = {
						sidebar = "tab", -- sidebar is when position = left or right
						current = "window", -- current is when position = current
					},
					filtered_items = {
						hide_dotfiles = false,
						hide_gitignored = false,
					},
					follow_current_file = true,
					hijack_netrw_behavior = "open_current",
					commands = {
						image_wezterm = function(state)
							local node = state.tree:get_node()
							if node.type == "file" then
								require("image_preview").PreviewImage(node.path)
							end
						end,
					},
					window = {
						mappings = {
							["P"] = "image_wezterm",
						},
					},
				},
				window = {
					width = 36,
				},
			})
		end,
	},
}
