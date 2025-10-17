return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
    config = function()
      local wk = require("which-key")
      local telescope = require("telescope.builtin")
      require("telescope").load_extension("file_browser")
      wk.add({
        { "<leader>f", group = "files" },
        {
          "<leader>ff",
          function()
            require("telescope").extensions.file_browser.file_browser({
              path = vim.fn.getcwd(),
              select_buffer = true,
            })
          end,
          desc = "find",
        },
        { "<leader>fs", "<cmd>write<cr>", desc = "save" },
        { "<leader>fS", "<cmd>wall<cr>", desc = "save all" },
      })
    end,
  },

  {
    "nvim-tree/nvim-tree.lua",
    config = function()
      require("nvim-tree").setup({
        view = { width = 35 },
        git = { ignore = false },
        filters = { dotfiles = false, git_ignored = false },
        actions = {
          open_file = {
            quit_on_open = true,
          },
        },
      })
      local wk = require("which-key")
      wk.add({
        { "<leader>d", "<cmd>NvimTreeFindFile<cr>", desc = "dir" },
      })
    end,
  },
}
