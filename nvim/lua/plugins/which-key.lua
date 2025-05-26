return{
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
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
  config = function()
  
    local wk = require("which-key")
    local telescope = require("telescope.builtin")

    wk.add({
      { "<leader>f", group = "files" },
      { "<leader>ff", telescope.find_files, desc = "find" },
      { "<leader>fs", "<cmd>write<cr>", desc = "save" },
      { "<leader>d", "<cmd>NvimTreeFindFile<cr>", desc = "dir" }
    })

    wk.add({
      { "<leader>q", group = "quit" },
      { "<leader>qq", "<cmd>qa<cr>", desc = "quit" },
      { "<leader>q!", "<cmd>qa!<cr>", desc = "quit without saving" },
    })

    wk.add({
    { "<leader>w", group = "window" },
    { "<leader>w+", "<C-w>+", desc = "increase height" },
    { "<leader>w_", "<C-w>-", desc = "decrease height" },
    { "<leader>w/", "<C-w>v", desc = "split right" },
    { "<leader>w-", "<C-w>s", desc = "split down" },
    { "<leader>w<", "<C-w><", desc = "decrease width" },
    { "<leader>w=", "<C-w>=", desc = "equalize" },
    { "<leader>w>", "<C-w>>", desc = "increase width" },
    { "<leader>wH", "<C-w>H", desc = "move far left" },
    { "<leader>wJ", "<C-w>J", desc = "move to bottom`" },
    { "<leader>wK", "<C-w>K", desc = "move to top" },
    { "<leader>wL", "<C-w>L", desc = "move to far right" },
    { "<leader>wd", "<C-w>q", desc = "delete window" },
    { "<leader>wh", "<C-w>h", desc = "go left" },
    { "<leader>wj", "<C-w>j", desc = "go down" },
    { "<leader>wk", "<C-w>k", desc = "go up" },
    { "<leader>wl", "<C-w>l", desc = "go right" },
    { "<leader>wo", "<C-w>o", desc = "only this window" },
    { "<leader>wr", "<C-w>r", desc = "rotate windows" },
    { "<leader>wt", "<C-w>T", desc = "break into tab" },
    { "<leader>wx", "<C-w>x", desc = "swap windows" },
  })
  end,
},

}