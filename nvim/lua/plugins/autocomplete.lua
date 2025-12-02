return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "R-nvim/cmp-r",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
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
      require("cmp_r").setup({
        filetypes = { "r", "rmd", "quarto" },
        doc_width = 60,
      })
    end,
  },
}
