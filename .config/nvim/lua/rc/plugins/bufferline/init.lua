require('bufferline').setup {
  options = {
    mode = 'buffers',
    diagnostics = 'nvim_lsp',
    show_close_icon = false,
  }
}
vim.keymap.set('n', '<Tab>', '<Cmd>BufferLineCycleNext<CR>', {})
vim.keymap.set('n', '<S-Tab>', '<Cmd>BufferLineCyclePrev<CR>', {})
