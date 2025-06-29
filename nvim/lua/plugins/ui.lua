return {
  {
    "GustavoPrietoP/doom-themes.nvim",
    priority = 1000, -- load before everything else
    config = function()
      vim.opt.termguicolors = true
      vim.o.background = "dark"
      vim.cmd([[colorscheme doom-dracula]])
    end,
  },
}
