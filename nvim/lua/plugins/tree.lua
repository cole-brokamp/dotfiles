return {
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
            resize_window = false,
            window_picker = { enable = false },
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
