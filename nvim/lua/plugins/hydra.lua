return {
  {
    "nvimtools/hydra.nvim",
    config = function()
      local Hydra = require("hydra")

      _G.hydra_window_rearrange = Hydra({
        name = "window rearrange",
        mode = "n",
        config = {
          exit = false,
          foreign_keys = nil,
        },
        heads = {
          { ">", "<cmd>vertical resize +2<cr>" },
          { "<", "<cmd>vertical resize -2<cr>" },
          { "+", "<cmd>resize +2<cr>" },
          { "-", "<cmd>resize -2<cr>" },
          { "=", "<cmd>wincmd =<cr>" },
          { "<Left>", "<cmd>wincmd r<cr>" },
          { "<Right>", "<cmd>wincmd R<cr>" },
          { "<Esc>", nil, { exit = true } },
        },
      })
      vim.keymap.set("n", "<leader>wr", function()
        hydra_window_rearrange:activate()
      end, { desc = "rearrange" })
    end,
  },
}
