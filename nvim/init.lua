vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- ui
vim.cmd([[
  hi Normal guibg=NONE ctermbg=NONE
  hi NormalNC guibg=NONE ctermbg=NONE
  hi EndOfBuffer guibg=NONE ctermbg=NONE
  hi SignColumn guibg=NONE ctermbg=NONE
  hi VertSplit guibg=NONE ctermbg=NONE
]])
vim.api.nvim_set_hl(0, "CursorLine", { underline = true })
vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"

-- indent/tabs
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.smartindent = true
vim.opt.autoindent = true

-- search
vim.opt.ignorecase = TRUE
vim.opt.smartcase = TRUE
vim.opt.incsearch = true
vim.opt.hlsearch = true

vim.opt.autoread = true
vim.opt.ttyfast = true
vim.opt.lazyredraw = true

-- scrolling
vim.keymap.set("n", "<C-e>", "3<C-e>", { noremap = true })
vim.keymap.set("n", "<C-y>", "3<C-y>", { noremap = true })
vim.opt.scrolloff = 4

-- perf
vim.opt.timeoutlen = 500
vim.opt.updatetime = 300

-- editing
vim.opt.iskeyword = vim.opt.iskeyword - { "_" }
vim.opt.clipboard = "unnamedplus"

-- markdown formatting
vim.api.nvim_set_hl(0, "markdownH1", { fg = "#fff0bd", bold = true })
vim.api.nvim_set_hl(0, "markdownH2", { fg = "#5c7ae0", italic = true })
vim.api.nvim_set_hl(0, "markdownH3", { fg = "#23a39a" })

-- misc
vim.opt.hidden = true
vim.opt.mouse = "a"

-- load other files
require("config/autocmds")
require("plugins")
