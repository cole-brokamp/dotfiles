return {
  {
    "nvim-telescope/telescope.nvim",
    config = function()
      local wk = require("which-key")
      wk.add({ { "<Leader>s", group = "search" } })
      local telescope = require("telescope.builtin")
      vim.keymap.set("n", "<Leader>ss", telescope.current_buffer_fuzzy_find, { desc = "search buffer" })
      vim.keymap.set("n", "<Leader>sS", telescope.live_grep, { desc = "search project" })
      vim.keymap.set("n", "<Leader>sr", query_replace, { desc = "search and replace" })
    end,
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").load_extension("file_browser")
      require("which-key").add({
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
      })
    end,
  },
}
