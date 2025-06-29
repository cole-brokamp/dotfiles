return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      local wk = require("which-key")
      wk.add({
        { "<leader>g", group = "git" },
        { "<leader>gs", "<cmd>Neogit kind=split<cr>", desc = "status" },
        { "<leader>gh", "<cmd>OpenInGHRepo<cr>", desc = "github" },
      })
    end,
  },
  { "Almo7aya/openingh.nvim" },
}

