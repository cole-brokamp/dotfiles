return {
  {
    "GustavoPrietoP/doom-themes.nvim",
    priority = 1000, -- load before everything else
    config = function()
      local function set_markdown_highlights()
        vim.api.nvim_set_hl(0, "@markup.heading.1.markdown", { fg = "#fff0bd", bold = true })
        vim.api.nvim_set_hl(0, "@markup.heading.2.markdown", { fg = "#5c7ae0", italic = true })
        vim.api.nvim_set_hl(0, "@markup.heading.3.markdown", { fg = "#23a39a" })
        vim.api.nvim_set_hl(0, "@markup.heading.4.markdown", { fg = "#8be9fd" })
        vim.api.nvim_set_hl(0, "@markup.heading.5.markdown", { fg = "#ff79c6" })
        vim.api.nvim_set_hl(0, "@markup.heading.6.markdown", { fg = "#bd93f9" })

        vim.api.nvim_set_hl(0, "@markup.list.markdown", { fg = "#ffb86c" })
        vim.api.nvim_set_hl(0, "@markup.list.checked.markdown", { fg = "#50fa7b", bold = true })
        vim.api.nvim_set_hl(0, "@markup.list.unchecked.markdown", { fg = "#ffb86c" })
        vim.api.nvim_set_hl(0, "@markup.quote.markdown", { fg = "#8be9fd", italic = true })
        vim.api.nvim_set_hl(0, "@markup.link.label.markdown", { fg = "#ff79c6", underline = true })
        vim.api.nvim_set_hl(0, "@markup.link.url.markdown", { fg = "#50fa7b", underline = true })
        vim.api.nvim_set_hl(0, "@markup.raw.markdown", { fg = "#f1fa8c" })
        vim.api.nvim_set_hl(0, "@markup.raw.block.markdown", { fg = "#f1fa8c" })
        vim.api.nvim_set_hl(0, "@markup.strong.markdown", { bold = true, fg = "#ff79c6" })
        vim.api.nvim_set_hl(0, "@markup.italic.markdown", { italic = true, fg = "#8be9fd" })
        vim.api.nvim_set_hl(0, "@punctuation.special.markdown", { fg = "#6272a4" })
        vim.api.nvim_set_hl(0, "@label.markdown", { fg = "#bd93f9" })
      end

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
      set_markdown_highlights()
      vim.api.nvim_create_autocmd("ColorScheme", {
        pattern = "*",
        callback = function()
          set_split_highlights()
          set_markdown_highlights()
        end,
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
}
