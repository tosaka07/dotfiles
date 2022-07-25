-- COMMANDS                    MODES ~
-- :map   :noremap  :unmap     Normal, Visual, Select, Operator-pending
-- :nmap  :nnoremap :nunmap    Normal
-- :vmap  :vnoremap :vunmap    Visual and Select
-- :smap  :snoremap :sunmap    Select
-- :xmap  :xnoremap :xunmap    Visual
-- :omap  :onoremap :ounmap    Operator-pending
-- :map!  :noremap! :unmap!    Insert and Command-line
-- :imap  :inoremap :iunmap    Insert
-- :lmap  :lnoremap :lunmap    Insert, Command-line, Lang-Arg
-- :cmap  :cnoremap :cunmap    Command-line
-- :tmap  :tnoremap :tunmap    Terminal

-- -------------------------------------------------------------------------------------------|
--  Modes     | Normal | Insert | Command | Visual | Select | Operator | Terminal | Lang-Arg |
--  [nore]map  |    @   |   -    |    -    |   @    |   @    |    @     |    -     |    -     |
--  n[nore]map |    @   |   -    |    -    |   -    |   -    |    -     |    -     |    -     |
--  n[orem]ap! |    -   |   @    |    @    |   -    |   -    |    -     |    -     |    -     |
--  i[nore]map |    -   |   @    |    -    |   -    |   -    |    -     |    -     |    -     |
--  c[nore]map |    -   |   -    |    @    |   -    |   -    |    -     |    -     |    -     |
--  v[nore]map |    -   |   -    |    -    |   @    |   @    |    -     |    -     |    -     |
--  x[nore]map |    -   |   -    |    -    |   @    |   -    |    -     |    -     |    -     |
--  s[nore]map |    -   |   -    |    -    |   -    |   @    |    -     |    -     |    -     |
--  o[nore]map |    -   |   -    |    -    |   -    |   -    |    @     |    -     |    -     |
--  t[nore]map |    -   |   -    |    -    |   -    |   -    |    -     |    @     |    -     |
--  l[nore]map |    -   |   @    |    @    |   -    |   -    |    -     |    -     |    @     |
-- -------------------------------------------------------------------------------------------"

local keymap = vim.keymap

-- jj to Escape
keymap.set("i", "jj", "<ESC>", { noremap = true, silent = true })
