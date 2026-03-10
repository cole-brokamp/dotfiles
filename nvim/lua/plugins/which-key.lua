return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "buffer local keymaps (which-key)",
      },
    },
    config = function()
      local wk = require("which-key")

      wk.add({
        { "<leader>q", group = "quit" },
        { "<leader>qq", "<cmd>qa<cr>", desc = "quit" },
        { "<leader>q!", "<cmd>qa!<cr>", desc = "quit without saving" },
      })

      wk.add({
        { "<leader>b", group = "buffers" },
        { "<leader>bb", "<cmd>Telescope buffers<CR>", desc = "buffers" },
        { "<leader>bd", "<cmd>bp | bd #<CR>", desc = "delete" },
        { "<leader>bD", "<cmd>bd!<CR>", desc = "kill" },
        { "<leader>br", "<cmd>e!<CR>", desc = "reload" },
        { "<leader>bn", "<cmd>enew<CR>", desc = "new" },
        { "<leader>bs", "<cmd>new | setlocal buftype=nofile bufhidden=hide noswapfile<CR>", desc = "scratch" },
        { "<leader>bn", "<cmd>bn<CR>", desc = "next" },
        { "<leader>bp", "<cmd>bp<CR>", desc = "previous" },
        { "<leader><Tab>", "<cmd>b#<CR>", desc = "last buffer" },
      })

      wk.add({
        { "<leader>f", group = "files" },
        { "<leader>fs", "<cmd>write<cr>", desc = "save" },
        { "<leader>fS", "<cmd>wall<cr>", desc = "save all" },
      })

      wk.add({
        { "<leader>w", group = "window" },
        { "<leader>wh", "<C-w>h", desc = "go left" },
        { "<leader>wj", "<C-w>j", desc = "go down" },
        { "<leader>wk", "<C-w>k", desc = "go up" },
        { "<leader>wl", "<C-w>l", desc = "go right" },
        { "<leader>w/", "<C-w>v", desc = "split right" },
        { "<leader>w-", "<C-w>s", desc = "split down" },
        { "<leader>wH", "<C-w>H", desc = "move far left" },
        { "<leader>wJ", "<C-w>J", desc = "move to bottom`" },
        { "<leader>wK", "<C-w>K", desc = "move to top" },
        { "<leader>wL", "<C-w>L", desc = "move to far right" },
        { "<leader>wd", "<C-w>q", desc = "delete window" },
        { "<leader>ws", "<C-w>x", desc = "swap windows" },
      })
    end,
  },
}
