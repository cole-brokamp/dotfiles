return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "R-nvim/cmp-r",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-cmdline",
    },
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        completion = {
          autocomplete = false,
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "cmp_r" },
          { name = "path" },
          { name = "buffer" },
          { name = "nvim_lsp_signature_help" },
        }),
        mapping = {
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<C-n>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            else
              cmp.complete()
            end
          end, { "i", "s" }),
          ["<C-p>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            else
              cmp.complete()
            end
          end, { "i", "s" }),
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.close()
            else
              fallback()
            end
          end, { "i", "s" }),
        },
      })

      cmp.setup.filetype("lua", {
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "nvim_lua" },
        }, {
          { name = "buffer" },
          { name = "path" },
        }),
      })

      cmp.setup.cmdline("/", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
      })

      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })
      require("cmp_r").setup({
        filetypes = { "r", "rmd", "quarto" },
        doc_width = 60,
      })
    end,
  },
}
