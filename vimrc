set nocompatible
set autoread

set cursorline
set history=1000
set autoindent
set smartindent
set backspace=indent,eol,start
" set number
set showmatch

" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed
" Enhance command-line completion
set wildmenu

" Optimize for fast terminal connections
set ttyfast
set lazyredraw

" Add the g flag to search/replace by default
set gdefault
" Highlight searches
set hlsearch
" Ignore case of searches
set ignorecase
" Highlight dynamically as pattern is typed
set incsearch
"This unsets the "last search pattern" register by hitting return
nnoremap <CR> :noh<CR><CR>

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'scrooloose/nerdTree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'yggdroot/indentline'
Plugin 'VundleVim/Vundle.vim'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'altercation/vim-colors-solarized'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Plugin jalvesaq/Nvim-R
call vundle#end()

filetype plugin indent on

" vim-colors-solarized settings
let g:solarized_termtrans=1
let g:solarized_termcolors=256
syntax enable
set background=dark
colorscheme solarized

""" nerdtree settings
map <C-n> :NERDTreeTabsToggle<CR>
" close vim if only window left is nerdtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=1
let NERDTreeDirArrowExpandable = '▷'
let NERDTreeDirArrowCollapsible = '▼'
let g:nerdtree_tabs_open_on_console_startup=2 " open NT if vim opened with a directory
let g:nerdtree_tabs_autofind=1 "auto find and select currently opened file in NT

""" airline settings
set laststatus=2
let g:airline_theme='solarized' "autoset theme theme based on matching colorscheme
let g:airline#extensions#tabline#enabled = 1 " enable airline tabline
let g:airline#extensions#tabline#tab_min_count = 0 " only show tabline if tabs are being used (more than 1 tab open)
"let g:airline#extensions#tabline#show_buffers = 0 " do not show open buffers in tabline
"let g:airline#extensions#tabline#show_splits = 0
inoremap jk <esc>
