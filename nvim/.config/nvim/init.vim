syntax on

set noerrorbells

"Window settings
set nu
set relativenumber

"Indentation
set tabstop=4 softtabstop=4
set shiftwidth=4
set smartindent

"Search settings
set smartcase
set nohls
set incsearch

"File/buffer management
set noswapfile
set nobackup
set undodir=~/.cache/nvim/undo
set undofile
set autoread

"Custom Mappings
let mapleader = " "
inoremap jk <Esc>
inoremap <C-s> <Esc>:w<CR>gi
nnoremap zz :w<CR>
noremap <C-p> "+p
noremap <C-y> "+y
nnoremap <C-z> <nop>

autocmd BufWritePost *.tex !pdflatex <afile>
autocmd BufWritePost *.py !python <afile>

"Global variables
let g:python3_host_prog='/usr/bin/python3'
let g:python_host_prog='/usr/bin/python'

"Plugins
call plug#begin('~/.local/share/nvim/plugged')
Plug 'morhetz/gruvbox'
Plug 'mbbill/undotree'
Plug 'townk/vim-autoclose'
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-treesitter/nvim-treesitter', {'do' : 'TSUpdate'}
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'vimwiki/vimwiki'
if has('nvim')
	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
	Plug 'Shougo/deoplete.nvim'
	Plug 'roxma/nvim-yarp'
	Plug 'roxma/vim-hug-neovim-rpc'
endif
call plug#end()

"Global plugin variables
let g:deoplete#enable_at_startup=1
let g:gruvbox_contrast_light='hard'
let g:gruvbox_contrast_dark='hard'
let g:gruvbox_invert_selection=0
let g:gruvbox_italic=1
let g:gruvbox_italicize_comments=1
let g:vimwiki_list = [{'path': '~/Documents/Notes/', 'syntax': 'markdown', 'ext': 'md'}]
let g:markdown_folding = 1
set background=light

" colorscheme gruvbox
lua << EOF
	require'lspconfig'.clangd.setup{}
	require'lspconfig'.pyright.setup{}
EOF
