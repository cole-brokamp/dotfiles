return {
  {
    "GustavoPrietoP/doom-themes.nvim",
    priority = 1000, -- load before everything else
    config = function()
      vim.opt.termguicolors = true
      vim.opt.laststatus = 3 -- single global statusline so horizontal splits use separators
      vim.o.background = "dark"
      vim.cmd([[colorscheme doom-dracula]])

      local function set_split_highlights()
        vim.api.nvim_set_hl(0, "WinSeparator", { fg = "#ff79c6" })
        vim.api.nvim_set_hl(0, "VertSplit", { fg = "#44475a" })
        vim.opt.fillchars:append({
          vert = "┃",
          vertleft = "┣",
          vertright = "┫",
          verthoriz = "╋",
          horiz = "━",
          horizup = "┻",
          horizdown = "┳",
        })
      end

      set_split_highlights()
      vim.api.nvim_create_autocmd("ColorScheme", {
        pattern = "*",
        callback = set_split_highlights,
      })
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
