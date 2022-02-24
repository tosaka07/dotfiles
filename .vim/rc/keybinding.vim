"-------------------------------------------------------------------------------------------|
"  Modes     | Normal | Insert | Command | Visual | Select | Operator | Terminal | Lang-Arg |
" [nore]map  |    @   |   -    |    -    |   @    |   @    |    @     |    -     |    -     |
" n[nore]map |    @   |   -    |    -    |   -    |   -    |    -     |    -     |    -     |
" n[orem]ap! |    -   |   @    |    @    |   -    |   -    |    -     |    -     |    -     |
" i[nore]map |    -   |   @    |    -    |   -    |   -    |    -     |    -     |    -     |
" c[nore]map |    -   |   -    |    @    |   -    |   -    |    -     |    -     |    -     |
" v[nore]map |    -   |   -    |    -    |   @    |   @    |    -     |    -     |    -     |
" x[nore]map |    -   |   -    |    -    |   @    |   -    |    -     |    -     |    -     |
" s[nore]map |    -   |   -    |    -    |   -    |   @    |    -     |    -     |    -     |
" o[nore]map |    -   |   -    |    -    |   -    |   -    |    @     |    -     |    -     |
" t[nore]map |    -   |   -    |    -    |   -    |   -    |    -     |    @     |    -     |
" l[nore]map |    -   |   @    |    @    |   -    |   -    |    -     |    -     |    @     |
"-------------------------------------------------------------------------------------------"

"" Leader
let g:mapleader = "\<Space>"
noremap <Leader> <Nop>
noremap <dev>    <Nop>
map     m        <dev>

"" Zero (Move beginning toggle)
" noremap <expr> 0 getline('.')[0 : col('.') - 2] =~# '^\s\+$' ? '0' : '^'
noremap ^ 0

"" BackSpace
imap <C-h> <BS>
cmap <C-h> <BS>

"" Buffer
nnoremap <C-q> <C-^>

"" Save
nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>W :w!<CR>

"" Automatically indent with i and A
nnoremap <expr> i len(getline('.')) ? "i" : "\"_cc"
nnoremap <expr> A len(getline('.')) ? "A" : "\"_cc"

"" Indent
nnoremap < <<
nnoremap > >>
xnoremap < <gv
xnoremap > >gv|

"" Insert Mode(Exit)
inoremap <silent> jj <ESC>

