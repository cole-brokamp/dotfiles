return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    config = function()
      require("conform").setup({
        format_on_save = {
          lsp_fallback = false,
          timeout_ms = 1000,
        },
        formatters_by_ft = {
          lua = { "stylua" },
          yaml = { "prettier" },
          toml = { "taplo" },
          markdown = { "prettier" },
          rust = { "rustfmt" },
          just = { "just" },
        },
      })
    end,
  },
}
