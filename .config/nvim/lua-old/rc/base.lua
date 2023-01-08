local cmd = vim.cmd
local opt = vim.opt
local global = vim.g
local api = vim.api

-- Commands --
cmd([[command! PackerInstall packadd packer.nvim | lua require('rc/plugins').install()]])
cmd([[command! PackerUpdate packadd packer.nvim | lua require('rc/plugins').update()]])
cmd([[command! PackerSync packadd packer.nvim | lua require('rc/plugins').sync()]])
cmd([[command! PackerClean packadd packer.nvim | lua require('rc/plugins').clean()]])
cmd([[command! PackerCompile packadd packer.nvim | lua require('rc/plugins').compile()]])

-- Neovide
global.neovide_input_use_logo = 1
api.nvim_set_keymap('', '<D-v>', '+p<CR>', { noremap = true, silent = true })
api.nvim_set_keymap('!', '<D-v>', '<C-R>+', { noremap = true, silent = true })
api.nvim_set_keymap('t', '<D-v>', '<C-R>+', { noremap = true, silent = true })
api.nvim_set_keymap('v', '<D-v>', '<C-R>+', { noremap = true, silent = true })

global.neovide_transparency = 0.0
global.transparency = 0.9

global.neovide_background_color = '#1d1f21' .. string.format('%x', math.floor(255 * global.transparency))

global.neovide_floating_blur_amount_x = 2.0
global.neovide_floating_blur_amount_y = 2.0
global.neovide_confirm_quit = true
global.neovide_remember_window_size = true
global.neovide_cursor_vfx_mode = 'sonicboom'

global.gui_font_default_size = 14
global.gui_font_size = global.gui_font_default_size
global.gui_font_face = "UDEV Gothic 35NFLG"

RefreshGuiFont = function()
  opt.guifont = string.format("%s:h%s", global.gui_font_face, global.gui_font_size)
end

ResizeGuiFont = function(delta)
  global.gui_font_size = global.gui_font_size + delta
  RefreshGuiFont()
end

ResetGuiFont = function()
  global.gui_font_size = global.gui_font_default_size
  RefreshGuiFont()
end

ResetGuiFont()
