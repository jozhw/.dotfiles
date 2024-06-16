" Change mapleader
let mapleader = "\<Space>"
let maplocalleader = ","

" Set line numbers
set number

" Use jk to exit insert mode
inoremap jk <Esc>

" Map a key to return to netrw directory listing
nnoremap <leader>pv :Explore<CR>

" Clear search highlights
nnoremap <leader>nh :nohl<CR>

" Delete single character without copying into register
nnoremap x "_x

" Increment/decrement numbers
nnoremap <leader>+ <C-a>
nnoremap <leader>- <C-x>

" Window management
nnoremap <leader>sv <C-w>v
nnoremap <leader>sh <C-w>s
nnoremap <leader>se <C-w>=
nnoremap <leader>sx :close<CR>

nnoremap <leader>to :tabnew<CR>
nnoremap <leader>tx :tabclose<CR>
nnoremap <leader>tn :tabn<CR>
nnoremap <leader>tp :tabp<CR>

" Visual move lines
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

nnoremap J mzJ`z
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz
nnoremap n nzzzv
nnoremap N Nzzzv

nnoremap <leader>p "_dP

" Copy and delete mappings
nnoremap <leader>y "+y
vnoremap <leader>y "+y
nnoremap <leader>Y "+Y
vnoremap <leader>Y "+Y
nnoremap <leader>d "_d
vnoremap <leader>d "_d

inoremap <C-c> <Esc>

" Disable Q, as it's mapped to Ex mode by default
nnoremap Q <Nop>

" LSP formatting
nnoremap <leader>f :LspFormat<CR>

" Quickfix list mappings
nnoremap <C-k> :cnext<CR>zz
nnoremap <C-j> :cprev<CR>zz
nnoremap <leader>k :lnext<CR>zz
nnoremap <leader>j :lprev<CR>zz

" Search and replace
nnoremap <leader>s :%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>

" Make file executable
nnoremap <leader>x :!chmod +x %<CR>

" Open Packer configuration
nnoremap <leader>vpp :e ~/.dotfiles/nvim/.config/nvim/lua/theprimeagen/packer.lua<CR>

" Custom command
nnoremap <leader>mr :CellularAutomaton make_it_rain<CR>

" Load the last source (equivalent to :so)
nnoremap <leader><leader> :source %<CR>

" Exit insert mode in terminal
tnoremap <Esc> <C-\><C-n>
