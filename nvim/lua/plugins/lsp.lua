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

      if vim.fn.executable("R") == 1 then
        local has_languageserver = vim.fn.system({
          "R",
          "--slave",
          "-e",
          "cat(requireNamespace('languageserver', quietly=TRUE))",
        })

        if has_languageserver:find("TRUE", 1, true) then
          vim.lsp.config("r_language_server", {
            on_attach = on_attach,
            cmd = { "R", "--no-echo", "--slave", "-e", "languageserver::run()" },
            filetypes = { "r", "rmd", "rnoweb", "quarto" },
            root_dir = function(bufnr, on_dir)
              local root = vim.fs.root(bufnr, { ".git", ".Rproj", "DESCRIPTION" })
              if root then
                on_dir(root)
                return
              end

              local name = vim.api.nvim_buf_get_name(bufnr)
              if name ~= "" then
                on_dir(vim.fs.dirname(vim.fs.normalize(name)))
              else
                on_dir(vim.uv.cwd())
              end
            end,
          })

          vim.lsp.enable("r_language_server")
        else
          vim.notify(
            "Skipping r_language_server setup – install.packages('languageserver') not detected",
            vim.log.levels.WARN
          )
        end
      end
    end,
  },
}
