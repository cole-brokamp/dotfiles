return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "master",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "markdown",
          "markdown_inline",
          "r",
          "rnoweb",
          "yaml",
          "latex",
          "csv",
          "rust",
          "json",
          "just",
          "bash",
          "python",
          "sql",
          "tmux",
          "toml",
          "lua",
        },
        highlight = {
          enable = true,
        },
      })
    end,
  },
}
