
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

" show commands as they are typed
set showcmd
" Enhance command-line completion
set wildmenu

"complete options
set completeopt=noinsert,menuone,preview

" support mouse for resizing splits
set mouse=n
set ttymouse=xterm2

" Optimize for fast terminal connections
set ttyfast
set lazyredraw

" Add the g flag to search/replace by default
set gdefault
" Highlight dynamically as pattern is typed
set incsearch
" Highlight searches
set hlsearch
" Ignore case of searches
set ignorecase
" do not ignore capitalization if in all caps
set smartcase
"This unsets the 'last search pattern' register by hitting return
nnoremap <CR> :noh<CR><CR>

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'jalvesaq/Nvim-R'
Plugin 'scrooloose/nerdTree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
" Plugin 'yggdroot/indentline'
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
Plugin 'suan/vim-instant-markdown' " auto open preview of markdown files in background
Plugin 'chrisbra/csv.vim'
Plugin 'airblade/vim-gitgutter' " stage hunks, etc
    " jump between hunks with `[c` and `]c`
    " preview, stage, undo hunks with `<leader>hp`, `<leader>hs`, `<leader>hu`
Plugin 'cole-brokamp/vim-todo'
call vundle#end()

filetype plugin indent on

" vim-colors-solarized settings
let g:solarized_termtrans=1
let g:solarized_termcolors=256
syntax enable
set background=dark
colorscheme solarized

""" csv.vim settings
let g:csv_autocmd_arrange      = 1 " auto arrange when csv file opened
let g:csv_autocmd_arrange_size = 1024*10240 " only auto arrange on files < 10 MB
" let g:csv_strict_columns=1 "assume delim not in quotes or escaped (speed up)
" let g:csv_highlight_column = 'y'

hi CursorLine cterm=underline ctermfg=None ctermbg=None

""" nerdtree settings
map <C-n> :NERDTreeTabsToggle<CR>
" close vim if only window left is nerdtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=1
let NERDTreeDirArrowExpandable = '▷'
let NERDTreeDirArrowCollapsible = '▼'
let g:nerdtree_tabs_open_on_console_startup=2 " open NT if vim opened with a directory
let g:nerdtree_tabs_autofind=1 "auto find and select currently opened file in NT
let NERDTreeStatusLine=-1
let NERDTreeMinimalUI=1
let NERDTreeIgnore = ['\.swp$','\.DS_Store$','\.git$']

""" airline settings
set laststatus=2
let g:airline_theme='solarized' "autoset theme theme based on matching colorscheme
let g:airline#extensions#tabline#enabled = 1 " enable airline tabline
let g:airline#extensions#tabline#tab_min_count = 0
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#tabline#buffers_label = '🗂 '


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

" let g:markdown_folding=1 " markdown folding

" remap for escape
inoremap jk <esc>
vnoremap jk <esc>

" switch between buffers with tab and shift+tab
nnoremap  <silent>   <tab>  :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bnext<CR>
nnoremap  <silent> <s-tab>  :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bprevious<CR>

let maplocalleader = ","

""" Nvim-R options

" use my own tmux conf file
let R_notmuxconf = 1
let R_in_buffer = 0
let R_applescript = 0
let R_tmux_split = 1

let R_assign = 0
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine
