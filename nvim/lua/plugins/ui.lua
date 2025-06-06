return {
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
  "nvim-telescope/telescope.nvim",
  dependencies = { "nvim-lua/plenary.nvim" }
},

{
  "nvim-tree/nvim-tree.lua",
  config = function()
    require("nvim-tree").setup()
  end
},
}
