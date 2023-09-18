return {
	"williamboman/mason-lspconfig.nvim",
	"neovim/nvim-lspconfig",
	{
		"williamboman/mason.nvim",
		event = { "BufReadPost", "BufNewFile" },
		config = function()
			local mason = require("mason")
			local mason_lspconfig = require("mason-lspconfig")
			local lspconfig = require("lspconfig")
			-- Don't change order
			mason.setup()
			mason_lspconfig.setup({
				automatic_installation = true,
				ensure_installed = {
					"bashls",
					"cssls",
					"cssmodules_ls",
					"eslint",
					"html",
					"jsonls",
					"kotlin_language_server",
					"lua_ls",
					"ruby_ls",
					"rust_analyzer",
					"tailwindcss",
					"tsserver",
					"yamlls",
				},
			})

			local capabilities = require("cmp_nvim_lsp").default_capabilities()

			local on_attach = function(client, bufner)
				-- plugins
				require("lsp-format").on_attach(client)
				if client.server_capabilities.documentSymbolProvider then
					require("nvim-navic").attach(client, bufner)
				end

				-- manual keymaps
				local map = require("rc.util").map
				map("n", "<leader>lD", vim.lsp.buf.declaration, { desc = "Declaration", buffer = bufner })
				vim.keymap.set(
					"n",
					"<leader>lh",
					vim.lsp.buf.signature_help,
					{ desc = "Signature help", buffer = bufner }
				)
				vim.keymap.set("n", "<leader>lF", function()
					vim.lsp.buf.format({ async = true })
				end, { desc = "Format", buffer = bufner })
				-- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
				-- vim.keymap.set('n', 'gn', vim.lsp.buf.rename, bufopts)
				-- vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
				-- vim.keymap.set('n', 'ge', vim.diagnostic.open_float, bufopts)
				-- vim.keymap.set('n', 'g[', vim.diagnostic.goto_next, bufopts)
				-- vim.keymap.set('n', 'g]', vim.diagnostic.goto_prev, bufopts)
				-- vim.keymap.set('n', 'ga', vim.lsp.buf.code_action, bufopts)

				if client.server_capabilities.document_highlight then
					vim.cmd([[
            augroup lsp_document_highlight
              autocmd! * <buffer>
              autocmd CursorHold,CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
              autocmd CursorMoved,CursorMovedI <buffer> lua vim.lsp.buf.clear_references()
            augroup END
          ]])
				end
			end

			mason_lspconfig.setup_handlers({
				function(server_name)
					lspconfig[server_name].setup({
						capabilities = capabilities,
						on_attach = on_attach,
					})
				end,
				["lua_ls"] = function()
					lspconfig.lua_ls.setup({
						capabilities = capabilities,
						on_attach = on_attach,
						settings = {
							Lua = {
								diagnostics = {
									globals = { "vim" },
								},
							},
						},
					})
				end,
				["pyright"] = function()
					local python_root_files = {
						"WORKSPACE", -- added for Bazel; items below are from default config
						-- Rye の multiproject だとうまく動かない
						-- 'pyproject.toml',
						"requirements.lock",
						"setup.py",
						"setup.cfg",
						"requirements.txt",
						"Pipfile",
						"pyrightconfig.json",
					}
					lspconfig.pyright.setup({
						capabilities = capabilities,
						on_attach = on_attach,
						root_dir = lspconfig.util.root_pattern(unpack(python_root_files)),
						settings = {
							python = {
								venvPath = ".",
								pythonPath = "./.venv/bin/python",
								analysis = {
									typeCheckingMode = "basic",
									diagnosticMode = "workspace",
									extraPaths = "./libs/",
								},
								formatting = {
									blackPath = "./.venv/bin/black",
								},
							},
						},
					})
				end,
				["ruff_lsp"] = function()
					local python_root_files = {
						"WORKSPACE", -- added for Bazel; items below are from default config
						-- Rye の multiproject だとうまく動かない
						-- 'pyproject.toml',
						"requirements.lock",
						"setup.py",
						"setup.cfg",
						"requirements.txt",
						"Pipfile",
						"pyrightconfig.json",
					}
					lspconfig["ruff_lsp"].setup({
						capabilities = capabilities,
						on_attach = on_attach,
						root_dir = lspconfig.util.root_pattern(unpack(python_root_files)),
					})
				end,
			})
		end,
	},
	{
		"jose-elias-alvarez/null-ls.nvim",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = { "mason.nvim" },
		opts = function()
			local nls = require("null-ls")
			return {
				root_dir = require("null-ls.utils").root_pattern(".null-ls-root", ".neoconf.json", "Makefile", ".git"),
				sources = {
					nls.builtins.formatting.stylua,
					nls.builtins.formatting.black,
				},
				on_attach = function(client, _)
					require("lsp-format").on_attach(client)
				end,
			}
		end,
	},
	{
		"kkharji/lspsaga.nvim",
		event = { "LspAttach" },
		config = function()
			require("lspsaga").setup()

			local map = require("rc.util").map
			map("n", "K", "<cmd>Lspsaga hover_doc<CR>", { desc = "Hover doc" })
			map("n", "<leader>lf", "<cmd>Lspsaga lsp_finder<CR>", { desc = "Lsp Finder" })
			map("n", "<leader>la", "<cmd>Lspsaga code_action<CR>", { desc = "Code Action" })
			map("v", "<leader>lA", "<cmd><C-U>Lspsaga range_code_action<CR>", { desc = "Range Code Action" })
			map("n", "<leader>lR", "<cmd>Lspsaga rename<CR>", { desc = "Rename" })
			map("n", "<leader>lp", "<cmd>Lspsaga preview_definition<CR>", { desc = "Preview definition" })
		end,
	},
	{
		"lukas-reineke/lsp-format.nvim",
		config = function()
			require("lsp-format").setup()
		end,
	},
	{
		"SmiteshP/nvim-navic",
		config = function()
			require("nvim-navic").setup()
		end,
	},
	{
		"j-hui/fidget.nvim",
		config = function()
			require("fidget").setup()
		end,
	},
}
