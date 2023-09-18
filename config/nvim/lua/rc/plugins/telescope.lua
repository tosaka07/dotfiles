local M = {
  "nvim-telescope/telescope.nvim",
  cmd = { "Telescope" },
  dependencies = {
    { "nvim-telescope/telescope-fzf-native.nvim",  build = "make" },
    -- { "nvim-telescope/telescope-frecency.nvim" },
    { "nvim-telescope/telescope-file-browser.nvim" },
  },
  opts = function(_, opts)
    local function flash(prompt_bufnr)
      require("flash").jump({
        pattern = "^",
        highlight = { label = { after = { 0, 0 } } },
        search = {
          mode = "search",
          exclude = {
            function(win)
              return vim.bo[vim.api.nvim_win_get_buf(win)].filetype ~= "TelescopeResults"
            end,
          },
        },
        action = function(match)
          local picker = require("telescope.actions.state").get_current_picker(prompt_bufnr)
          picker:set_selection(match.pos[1] - 1)
        end,
      })
    end
    opts.defaults = vim.tbl_deep_extend("force", opts.defaults or {}, {
      mappings = {
        n = { s = flash },
        i = { ["<c-s>"] = flash },
      },
    })
  end,
  config = function()
    local telescope = require("telescope")
    telescope.setup()
    telescope.load_extension("fzf")
    -- telescope.load_extension("frecency")
    telescope.load_extension("file_browser")
  end
}

return M
