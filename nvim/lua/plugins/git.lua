return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      signs = {
        add = { text = "│" },
        change = { text = "│" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
        untracked = { text = "┆" },
      },
      signcolumn = true,
      numhl = false,
      linehl = false,
      current_line_blame = false,
    },
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("which-key").add({
        { "<leader>g", group = "git" },
        { "<leader>gs", "<cmd>Neogit kind=split<cr>", desc = "status" },
      })
    end,
  },
  {
    "Almo7aya/openingh.nvim",
    config = function()
      require("which-key").add({
        { "<leader>gh", "<cmd>OpenInGHRepo<cr>", desc = "github" },
      })
    end,
  },
}
