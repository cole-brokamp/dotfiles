return {
  "nvim-treesitter/nvim-treesitter",
  config = function()
    require("lazy").setup({
      { "nvim-treesitter/nvim-treesitter", branch = "main", lazy = false, build = ":TSUpdate" },
    })
  end,
}
