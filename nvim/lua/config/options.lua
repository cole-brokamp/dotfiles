vim.opt.termguicolors = true

-- clear out background coloring
vim.cmd [[
  hi Normal guibg=NONE ctermbg=NONE
  hi NormalNC guibg=NONE ctermbg=NONE
  hi EndOfBuffer guibg=NONE ctermbg=NONE
  hi SignColumn guibg=NONE ctermbg=NONE
  hi VertSplit guibg=NONE ctermbg=NONE
]]

vim.api.nvim_set_hl(0, "markdownH1", { fg = "#fff0bd", bold = true })
vim.api.nvim_set_hl(0, "markdownH2", { fg = "#5c7ae0", italic = true })
vim.api.nvim_set_hl(0, "markdownH3", { fg = "#23a39a" })
