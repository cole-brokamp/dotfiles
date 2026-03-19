return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local ok_cmp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
      local cmp_capabilities = ok_cmp and cmp_nvim_lsp.default_capabilities() or nil
      local default_hover = vim.lsp.handlers["textDocument/hover"]
      local ok_wk, which_key = pcall(require, "which-key")

      if ok_wk then
        which_key.add({
          { "<leader>l", group = "lsp" },
        })
      end

      vim.keymap.set("n", "<leader>lh", vim.lsp.buf.hover, { desc = "hover" })
      vim.keymap.set("n", "<leader>ld", vim.lsp.buf.definition, { desc = "definition" })
      vim.keymap.set("n", "<leader>lD", vim.lsp.buf.type_definition, { desc = "type definition" })
      vim.keymap.set("n", "<leader>lr", vim.lsp.buf.references, { desc = "references" })
      vim.keymap.set("n", "<leader>lR", vim.lsp.buf.rename, { desc = "rename" })
      vim.keymap.set({ "n", "v" }, "<leader>la", vim.lsp.buf.code_action, { desc = "code action" })
      vim.keymap.set("n", "<leader>ls", vim.lsp.buf.signature_help, { desc = "signature help" })
      vim.keymap.set("n", "<leader>lo", vim.lsp.buf.document_symbol, { desc = "document symbols" })
      vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format, { desc = "format" })
      vim.keymap.set("n", "<leader>le", vim.diagnostic.open_float, { desc = "line diagnostics" })
      vim.keymap.set("n", "<leader>ln", vim.diagnostic.goto_next, { desc = "next diagnostic" })
      vim.keymap.set("n", "<leader>lp", vim.diagnostic.goto_prev, { desc = "previous diagnostic" })

      vim.lsp.handlers["textDocument/hover"] = function(err, result, ctx, config)
        local bufnr, winnr = default_hover(err, result, ctx, config)

        if bufnr and winnr then
          local close_hover = function()
            if vim.api.nvim_win_is_valid(winnr) then
              vim.api.nvim_win_close(winnr, true)
            end
          end

          vim.keymap.set("n", "q", close_hover, { buffer = bufnr, silent = true, nowait = true })
          vim.keymap.set("n", "<Esc>", close_hover, { buffer = bufnr, silent = true, nowait = true })
        end

        return bufnr, winnr
      end

      local function on_attach(client, bufnr)
        if client.name == "r_language_server" then
          client.server_capabilities.documentFormattingProvider = false
        end
      end

      vim.lsp.config("air", {
        on_attach = on_attach,
        capabilities = cmp_capabilities,
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
            capabilities = cmp_capabilities,
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
