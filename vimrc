set nocompatible
set autoread

" faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

set relativenumber
set number
set cursorline

set history=1000
set autoindent
set smartindent
set backspace=indent,eol,start
set showmatch

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" show when leader key is pressed
set showcmd
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
Plugin 'qpkorr/vim-bufkill' "close buffers leaving split open with :BW
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-commentary' "use gcc or gc <target> to comment out lines
Plugin 'tpope/vim-fugitive'
Plugin 'ervandew/supertab' "completions with tab
Plugin 'cole-brokamp/vim-todo'
call vundle#end()

filetype plugin indent on

" vim-colors-solarized settings
let g:solarized_termtrans=1
let g:solarized_termcolors=256
syntax enable
set background=dark
colorscheme solarized

hi CursorLine cterm=underline ctermfg=None ctermbg=None

""" nerdtree settings
map <C-n> :NERDTreeTabsToggle<CR>
" close vim if only window left is nerdtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=0
let NERDTreeDirArrowExpandable = '▷'
let NERDTreeDirArrowCollapsible = '▼'
let g:nerdtree_tabs_open_on_console_startup=2 " open NT if vim opened with a directory
let g:nerdtree_tabs_autofind=1 "auto find and select currently opened file in NT
let NERDTreeStatusLine=-1
let NERDTreeMinimalUI=1
let NERDTreeIgnore = ['\.swp$','\.DS_Store$']

""" airline settings
set laststatus=2
let g:airline_theme='solarized' "autoset theme theme based on matching colorscheme
let g:airline#extensions#tabline#enabled = 1 " enable airline tabline
let g:airline#extensions#tabline#tab_min_count = 0
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#tabline#buffers_label = 'b'


" configure separators for the tabline only. >
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''


let g:airline#extensions#csv#column_display = 'Name'
let g:airline#extensions#csv#enabled = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''


" remap for escape
inoremap jk <esc>
vnoremap jk <esc>

" switch between buffers with tab and shift+tab
nnoremap  <silent>   <tab>  :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bnext<CR>
nnoremap  <silent> <s-tab>  :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bprevious<CR>
