return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    config = function()
      require("conform").setup({
        format_on_save = {
          lsp_format = "fallback",
        },
        formatters_by_ft = {
          lua = { "stylua" },
          sh = { "shfmt" },
          yaml = { "prettier" },
          toml = { "taplo" },
          -- markdown = { "prettier" },
          rust = { "rustfmt" },
          just = { "just" },
          r = { "air" },
          quarto = { "injected" },
          rmd = { "injected" },
        },
      })
    end,
  },
}
