return{

{
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
  },
  {
  'stevearc/conform.nvim',
  opts = {},
},
}

-- require("lspconfig").air.setup({
--     on_attach = function(_, bufnr)
--         vim.api.nvim_create_autocmd("BufWritePre", {
--             buffer = bufnr,
--             callback = function(args)
-- 	      require("conform").format({ bufnr = args.buf })
--                 -- vim.lsp.buf.format()
--             end,
--         })
--     end,
-- })

-- require("conform").setup({
--     formatters_by_ft = {
--         quarto = { "injected" },
--         rmd = { "injected" },
--         r = { "air" },
--     },
-- })
-- vim.lsp.enable("air")


-- vim.lsp.config['R'] = {
--   cmd = { "air", "language-server" },
--   filetypes = { "r" },
--   root_markers = { "air.toml", ".air.toml", ".git" },
-- }

-- vim.lsp.enable('R')