require('telescope').setup {
  defaults = {
    mappings = {
      i = {
      },
      n = {
        ['q'] = require('telescope.actions').close
      }
    }
  },
  pickers = {
    find_files = {
      find_command = { "rg", "--files", "--hidden", "--glob", "!.git/*" },
    },
  },
  extensions = {
    file_browser = {
      theme = "dropdown",
      hijack_netrw = true,
      hidden = true,
      mappings = {
        ["i"] = {
        },
        ["n"] = {
        },
      },
    }
  }
}

local keymap = vim.keymap
local opts = { noremap = true, silent = true }
keymap.set('n', '<leader>fp', '<Cmd>Telescope find_files<CR>', opts)
keymap.set('n', '<leader>ff', '<Cmd>Telescope current_buffer_fuzzy_find fuzzy=false case_mode=ignore_case<CR>',
  opts)
keymap.set('n', '<leader>fF', '<Cmd>Telescope live_grep<CR>', opts)
keymap.set('n', '<leader>fg', '<Cmd>Telescope ghq list<CR>', opts)
keymap.set('n', '<leader>fG', '<Cmd>Telescope projects<CR>', opts)
keymap.set('n', '<leader>fb', '<Cmd>Telescope buffers<CR>', opts)
keymap.set('n', '<leader>fe', '<Cmd>Telescope file_browser<CR>', opts)
keymap.set('n', '<leader>fj', '<Cmd>Telescope diagnostics<CR>', opts)

vim.api.nvim_create_augroup("dart", {})
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  group = "dart",
  pattern = "*.dart",
  callback = function()
    keymap.set('n', '<leader>fm', function()
      require("telescope").extensions.flutter.commands()
    end)
  end
})
