return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local format_group = vim.api.nvim_create_augroup("ColeLspFormat", { clear = true })

      local function on_attach(client, bufnr)
        if client.server_capabilities.documentFormattingProvider then
          vim.api.nvim_clear_autocmds({ group = format_group, buffer = bufnr })
          vim.api.nvim_create_autocmd("BufWritePre", {
            group = format_group,
            buffer = bufnr,
            callback = function()
              vim.lsp.buf.format({ bufnr = bufnr })
            end,
          })
        end
      end

      vim.lsp.config("air", {
        on_attach = on_attach,
      })

      vim.lsp.enable("air")
    end,
  },
}
