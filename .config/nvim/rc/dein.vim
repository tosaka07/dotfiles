if &compatible
  set nocompatible " Be iMproved
endif

" dein directory {{{
let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
" }}}

" dein installation check {{{
if &runtimepath !~# '/dein.vim'
    if !isdirectory(s:dein_repo)
        execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo
    endif
    execute 'set runtimepath^=' . s:dein_repo
endif
" }}}


" Begin settings {{{
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  " .toml files
  let s:toml_dir = expand('~/.config/nvim/toml')
  let s:dein_toml = s:toml_dir . '/dein.toml'
  let s:dein_lazy_toml = s:toml_dir . '/lazy.toml'
  " let s:dein_ft_toml = s:toml_dir . '/filetype.toml'
  " let s:dein_ddc_toml = s:toml_dir . '/ddc.toml'
  " let s:dein_lsp_toml = s:toml_dir . '/lsp.toml'

  " read toml and cache
  call dein#load_toml(s:dein_toml, {'lazy': 0})
  call dein#load_toml(s:dein_lazy_toml, {'lazy': 1})
  " call dein#load_toml(s:dein_ft_toml, {'lazy': 1})
  " call dein#load_toml(s:dein_ddc_toml, {'lazy': 1})
  " call dein#load_toml(s:dein_lsp_toml, {'lazy': 1})

  " end settings
  call dein#end()
  call dein#save_state()
endif
" }}}

" Plugin installation check {{{
if dein#check_install()
    call dein#install()
endif
" }}}


