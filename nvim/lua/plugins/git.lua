return {
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
