return {
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
          show_close_icon = false,
          separator_style = "slant",
        },
      })

      local wk = require("which-key")
      wk.add({
        { "<leader>b", group = "buffers" },
        { "<leader>bb", "<cmd>Telescope buffers<CR>", desc = "buffers" },
        { "<leader>bd", "<cmd>bd<CR>", desc = "delete" },
        { "<leader>br", "<cmd>e!<CR>", desc = "reload" },
        { "<leader>bn", "<cmd>enew<CR>", desc = "new" },
        { "<leader>bs", "<cmd>new | setlocal buftype=nofile bufhidden=hide noswapfile<CR>", desc = "scratch" },
        { "<leader>bn", "<cmd>bn<CR>", desc = "next" },
        { "<leader>bp", "<cmd>bp<CR>", desc = "previous" },
      })

      -- Jump to last buffer (previous buffer in this window)
      vim.keymap.set("n", "<leader><Tab>", "<Cmd>b#<CR>", { desc = "Last buffer" })
    end,
  },
}
