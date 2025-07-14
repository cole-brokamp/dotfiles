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
}
