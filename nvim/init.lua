-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.opt.termguicolors = true

-- Setup lazy.nvim
require("lazy").setup({
  spec = {

  {
    "GustavoPrietoP/doom-themes.nvim",
    priority = 1000,             -- load before everything else
    config = function()
      vim.opt.termguicolors = true
      vim.o.background = "dark"
      vim.cmd [[colorscheme doom-dracula]]
    end,
  },

{
  "folke/which-key.nvim",
  event = "VeryLazy",
  opts = {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
},

{
  "nvim-telescope/telescope.nvim",
  dependencies = { "nvim-lua/plenary.nvim" }
},

{
  "nvim-tree/nvim-tree.lua",
  config = function()
    require("nvim-tree").setup()
  end
},

{
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",
    "nvim-telescope/telescope.nvim"
  },
},

{"Almo7aya/openingh.nvim"
},

{
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
  },
  {
  'stevearc/conform.nvim',
  opts = {},
},

  },
  install = { colorscheme = { "habamax" } },
  checker = { enabled = true },
})

-- clear out background coloring
vim.cmd [[
  hi Normal guibg=NONE ctermbg=NONE
  hi NormalNC guibg=NONE ctermbg=NONE
  hi EndOfBuffer guibg=NONE ctermbg=NONE
  hi SignColumn guibg=NONE ctermbg=NONE
  hi VertSplit guibg=NONE ctermbg=NONE
]]


local wk = require("which-key")
local telescope = require("telescope.builtin")

wk.add({
  { "<leader>f", group = "files" },
  { "<leader>ff", telescope.find_files, desc = "find" },
  { "<leader>fs", "<cmd>write<cr>", desc = "save" },
  { "<leader>d", "<cmd>NvimTreeFindFile<cr>", desc = "dir" }
})

wk.add({
  { "<leader>q", group = "quit" },
  { "<leader>qq", "<cmd>qa<cr>", desc = "quit" },
  { "<leader>q!", "<cmd>qa!<cr>", desc = "quit without saving" },
})

wk.add({
  { "<leader>g", group = "git" },
  { "<leader>gs", "<cmd>Neogit kind=split<cr>", desc = "status" },
  { "<leader>gh", "<cmd>OpenInGHRepo<cr>", desc = "github" },
})

wk.add({
    { "<leader>w", group = "window" },
    { "<leader>w+", "<C-w>+", desc = "increase height" },
    { "<leader>w_", "<C-w>-", desc = "decrease height" },
    { "<leader>w/", "<C-w>v", desc = "split right" },
    { "<leader>w-", "<C-w>s", desc = "split down" },
    { "<leader>w<", "<C-w><", desc = "decrease width" },
    { "<leader>w=", "<C-w>=", desc = "equalize" },
    { "<leader>w>", "<C-w>>", desc = "increase width" },
    { "<leader>wH", "<C-w>H", desc = "move far left" },
    { "<leader>wJ", "<C-w>J", desc = "move to bottom`" },
    { "<leader>wK", "<C-w>K", desc = "move to top" },
    { "<leader>wL", "<C-w>L", desc = "move to far right" },
    { "<leader>wd", "<C-w>q", desc = "delete window" },
    { "<leader>wh", "<C-w>h", desc = "go left" },
    { "<leader>wj", "<C-w>j", desc = "go down" },
    { "<leader>wk", "<C-w>k", desc = "go up" },
    { "<leader>wl", "<C-w>l", desc = "go right" },
    { "<leader>wo", "<C-w>o", desc = "only this window" },
    { "<leader>wr", "<C-w>r", desc = "rotate windows" },
    { "<leader>wt", "<C-w>T", desc = "break into tab" },
    { "<leader>wx", "<C-w>x", desc = "swap windows" },
  })

require("lspconfig").air.setup({
    on_attach = function(_, bufnr)
        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            callback = function(args)
	      require("conform").format({ bufnr = args.buf })
                -- vim.lsp.buf.format()
            end,
        })
    end,
})

require("conform").setup({
    formatters_by_ft = {
        quarto = { "injected" },
        rmd = { "injected" },
        r = { "air" },
    },
})
-- vim.lsp.enable("air")


-- vim.lsp.config['R'] = {
--   cmd = { "air", "language-server" },
--   filetypes = { "r" },
--   root_markers = { "air.toml", ".air.toml", ".git" },
-- }

-- vim.lsp.enable('R')

vim.api.nvim_set_hl(0, "markdownH1", { fg = "#fff0bd", bold = true })
vim.api.nvim_set_hl(0, "markdownH2", { fg = "#5c7ae0", italic = true })
vim.api.nvim_set_hl(0, "markdownH3", { fg = "#23a39a" })
vim.api.nvim_set_hl(0, "markdownH4", { fg = "#81c87c" })
