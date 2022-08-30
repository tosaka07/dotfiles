local dapui = require("dapui")

dapui.setup()

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<leader>bu', function() dapui.toggle() end, opts)
vim.keymap.set('n', '<leader>br', function() dapui.clear_breakpoints() end, opts)

-- TODO: Lua
vim.cmd([[
  function! s:trigger_hot_reload() abort
    silent exec '!kill -SIGUSR1 "$(pgrep -f flutter_tools.snapshot\ run)" &> /dev/null'
  endfunction
  function! s:trigger_hot_restart() abort
    silent exec '!kill -SIGUSR2 "$(pgrep -f flutter_tools.snapshot\ run)" &> /dev/null'
  endfunction
  function! s:flutter()
    command! FlutterHotReload call s:trigger_hot_reload()
    command! FlutterHotRestart call s:trigger_hot_restart()
    nnoremap <Leader>fm <cmd>lua require('telescope').extensions.flutter.commands()<CR>
  endfunction
  augroup dart
    autocmd!
    autocmd BufRead,BufNewFile *.dart call s:flutter()
  augroup end 
]])
