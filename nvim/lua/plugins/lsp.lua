return {
     {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("lspconfig").air.setup({
        on_attach = function(_, bufnr)
          vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            callback = function()
              vim.lsp.buf.format({ bufnr = bufnr })
            end,
          })
        end,
      })
    end,
  },

  {
  "stevearc/conform.nvim",
  config = function()
    require("conform").setup({
      format_on_save = {
	      lsp_ffallback = true,
	      timeout_ms = 500,
      },
      formatters_by_ft = {
        lua = { "stylua" },
        quarto = { "injected" },
        rmd = { "injected" },
        r = { "air" },
        yaml = { "prettier" },
        json = { "prettier" },
        csv = { "prettier" },
        markdown = { "prettier" },
        dockerfile = { "prettier" },
      },
    })
  end,
},
}

-- vim.lsp.enable("air")


-- vim.lsp.config['R'] = {
--   cmd = { "air", "language-server" },
--   filetypes = { "r" },
--   root_markers = { "air.toml", ".air.toml", ".git" },
-- }

-- vim.lsp.enable('R')
