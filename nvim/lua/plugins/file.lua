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
        { "<leader>ff", "<cmd>:Telescope file_browser path=%:p:h select_buffer=true<CR>", desc = "find" },
        { "<leader>fs", "<cmd>write<cr>", desc = "save" },
        { "<leader>fS", "<cmd>write<cr>", desc = "save all" },
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
      })
      local wk = require("which-key")
      wk.add({
        { "<leader>d", "<cmd>NvimTreeFindFile<cr>", desc = "dir" },
      })
    end,
  },
}
