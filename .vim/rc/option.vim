
" Encoding{{{

" Use utf-8
set encoding=utf-8

" Use file encodings whn loaded
set fileencodings=utf-8,sjis,iso-2022-jp,euc-jp

" Automatic line feed code recognition.
set fileformats=unix,dos,mac

" Disable backup.
set nobackup

" Don't make swap file.
set noswapfile

" Automatically load the file being edited
set autoread

" }}}


" Display {{{

" Display row number
set number

" Display relative rows number.
set relativenumber

" Display current row cursorline.
set cursorline

" True color terminal settings.
if exists('+termguicolors')
    let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif

" Transparency of pop-up menus such as completion
set pumblend=30

" }}}


" Folding {{{

set foldmethod=marker
set foldlevel=0
set foldcolumn=3

" }}}


" Search {{{

" Highlight search results
set hlsearch

" Incremental search.
" Search starts when you enter the first character of the search word.
set incsearch

" Search is not case sensitive
set ignorecase

" Searching in lowercase ignores uppercase and lowercase
set smartcase

" When the search progresses to the end of the file, search again from the beginning of the file.
set wrapscan

" }}}


" Indent {{{

" Smart indent.
set smartindent

" Insert tab with half-width space.
set expandtab

" The amount of blank space to insert with each command or smart indent.
set shiftwidth=2

" Tab width with 4 spaces.
set tabstop=2

" Insert a tab with 4 minutes of half-width space.
set softtabstop=2

" }}} 

set guifont=Cica:h16
set printfont=Cica:h12
set ambiwidth=double

