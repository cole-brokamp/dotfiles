local parsers = {
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
  "tsv",
  "lua",
}

local languages_by_filetype = {
  bash = "bash",
  csv = "csv",
  json = "json",
  just = "just",
  latex = "latex",
  lua = "lua",
  markdown = "markdown",
  python = "python",
  quarto = "markdown",
  r = "r",
  rmd = "markdown",
  rnoweb = "rnoweb",
  rust = "rust",
  sh = "bash",
  sql = "sql",
  tmux = "tmux",
  toml = "toml",
  tsv = "tsv",
  tex = "latex",
  yaml = "yaml",
}

return {
  {
    "neovim-treesitter/nvim-treesitter",
    dependencies = { "neovim-treesitter/treesitter-parser-registry" },
    lazy = false,
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter").install(parsers)

      for filetype, language in pairs(languages_by_filetype) do
        vim.treesitter.language.register(language, filetype)
      end

      vim.api.nvim_create_autocmd("FileType", {
        pattern = vim.tbl_keys(languages_by_filetype),
        callback = function(args)
          pcall(vim.treesitter.start, args.buf)
        end,
      })
    end,
  },
}
