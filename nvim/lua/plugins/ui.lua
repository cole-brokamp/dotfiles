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

  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    version = "*",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("bufferline").setup({
        options = {
          mode = "buffers",
          diagnostics = "nvim_lsp",
          show_buffer_close_icons = false,
          separator_style = "slant",
        },
      })
    end,
  },

  -- syntax highlighting
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("lazy").setup({
        { "nvim-treesitter/nvim-treesitter", branch = "main", lazy = false, build = ":TSUpdate" },
      })
    end,
  },
}
