local lazy = {}

function lazy.init()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "--branch=stable", -- remove this if you want to bootstrap to HEAD
      "https://github.com/folke/lazy.nvim.git",
      lazypath,
    })
  end
  vim.opt.rtp:prepend(lazypath)
end

function lazy.load()
  require("lazy").setup("rc.plugins", {
    defaults = { lazy = true },
    install = { colorscheme = { "tokyonight", "habamax" } },
    checker = { enabled = false },
    diff = {
      cmd = "terminal_git",
    },
  })
end

return lazy
