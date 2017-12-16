
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
set completeopt=menuone,preview

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
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary' "use gcc or gc <target> to comment out lines
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-rhubarb'
Plugin 'ervandew/supertab' "completions with tab
Plugin 'chrisbra/csv.vim'
Plugin 'airblade/vim-gitgutter' " stage hunks, etc
    " jump between hunks with `[c` and `]c`
    " preview, stage, undo hunks with `<leader>hp`, `<leader>hs`, `<leader>hu`
Plugin 'cole-brokamp/vim-todo'
Plugin 'roxma/vim-paste-easy' " auto set paste option when pasting
Plugin 'w0rp/ale'
call vundle#end()

filetype plugin indent on

" ALE settings
let g:syntastic_enable_r_lintr_checker = 1
let g:syntastic_r_checkers = ['lintr']
let g:syntastic_r_lintr_linters = "with_defaults(line_length_linter(120))"
let g:airline#extensions#ale#enabled = 1
let g:ale_sign_column_always = 1

" vim-colors-solarized settings
let g:solarized_termtrans=1
let g:solarized_termcolors=256
syntax enable
set background=dark
colorscheme solarized

""" gitgutter settings
" let g:gitgutter_sign_added = '🔷'
" let g:gitgutter_sign_modified = '🔶'
" let g:gitgutter_sign_removed = '❌'
" let g:gitgutter_sign_modified_removed = '💥'
nmap [h <Plug>GitGutterPrevHunk
nmap ]h <Plug>GitGutterNextHunk
nmap <Leader>hs <Plug>GitGutterStageHunk
nmap <Leader>hu <Plug>GitGutterUndoHunk
nmap <Leader>hp <Plug>GitGutterPreviewHunk

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
let g:airline#extensions#tabline#buffers_label = ''


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
" set foldcolumn=2

" remap for escape
inoremap jk <esc>

" switch between buffers with tab and shift+tab
nnoremap  <silent>   <tab>  :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bnext<CR>
nnoremap  <silent> <s-tab>  :if &modifiable && !&readonly && &modified <CR> :write<CR> :endif<CR>:bprevious<CR>

let mapleader = ","
let maplocalleader = ","

""" Supertab settings
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextTextOmniPrecedence = ['&omnifunc', '&completefunc']
let g:SuperTabContextTextMemberPatterns = ['\.', '>\?::', '$'] " use $ for R omnicompletion
let g:SuperTabCrMapping = 1

""" Nvim-R options

""" R Markdown and Latex
let R_texerr = 1                          " show summary of latex errors after compilation
let R_pdfviewer = 'Preview'               " application used to view PDF
let R_openpdf = 1                         " always open pdf after `knit()` is called
let R_openhtml = 1                        " always open html file after `knit()` is called
let R_rmd_environment = "new.env()"       " knit in a new env

""" R Session Behavior
let R_args = ['--no-save', '--quiet']     " start R with options
let R_wait_reply = 864000                 " set a long time to wait for R to reply
let R_commented_lines = 1                 " include commented lines when sending to R
let R_allnames = 1                        " show hidden names
let g:R_rmhidden = 0                      " don't include hidden objects when clearing R workspace

"""Tmux Setup
let R_notmuxconf = 1                      " use my own ~/.tmux.conf
let R_in_buffer = 0                       " use external terminal emulator
let R_applescript = 0                     " use tmux to send R code to REPL
let R_tmux_split = 1                      " use a split in tmux when starting R session
let R_editor_w = 64                       " desired width of R script buffer
let R_help_w = 46                         " desired width of R documentation buffer
let R_rconsole_width = 125                " number of columns of R Console
let R_tmux_title = "automatic"            " don't rename tmux splits 'NvimR'

""" Object Browser
let R_objbr_opendf = 0                    " hide data.frames elements
let R_objbr_openlist = 0                  " hide lists elements
let R_objbr_allnames = 1                  " Show .GlobalEnv hidden objects
let R_objbr_place = "script,left"         " place object browser at left side of vim pane
" let R_objbr_h = 20                        " set default height in lines

""" NVim-R Plugin Settings
let Rout_more_colors = 1                  " show more colors in .Rout files
let g:R_allnames = 1                      " include hidden objects in omnicompletion
let R_assign = 0                          " to diable undersore replacement
let R_nvimpager = "horizontal"            " open R help in horizontal split
let R_nvim_wd = 'yes'                     " start R in Vim's working directory
" R_after_start                           " System command to execute after R startup
let R_df_viewer = "CB::htable(%s)"        " Options for visualizing a data.frame or matrix

""" Function Arguments and Autocompletion
let R_show_args = 1                       " show preview window with arguments during omnicompletion
let R_args_in_stline = 1                  " show args in status line after `(` is pressed; end with `)`

"""  Add comment string <cr> instead of at fixed column value
autocmd FileType r setlocal formatoptions-=t formatoptions+=croql

""" Access Packages in Omnicompletion without R running
let R_start_libs = "base,stats,graphics,grDevices,utils,methods,tidyverse,CB,sp,rgeos,rgdal,sf"

""" Custom Mappings
let R_assign = 0
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine
