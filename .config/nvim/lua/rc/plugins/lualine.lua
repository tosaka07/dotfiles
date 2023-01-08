local M = {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
}

local function clock()
  return " " .. os.date("%H:%M")
end

function M.config()
  require("lualine").setup({
    options = {
      icons_enabled = true,
      globalstatus = true,
    }
  })
end

return M
