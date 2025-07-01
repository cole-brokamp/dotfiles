return {
  {
    "R-nvim/R.nvim",
    ft = { "r", "rmd", "quarto" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      -- Create a table with the options to be passed to setup()
      ---@type RConfigUserOpts
      local opts = {
        Rout_more_colors = true,
        R_args = { "--quiet", "--no-save" },
        clear_line = true,
        min_editor_width = 82,
        rconsole_width = 120,
        disable_cmds = {
          "RCustomStart",
          "RSourceDir",
          "RBuildTags",
          "RSPlot",
          "RMapsDesc",
          "RSaveClose",
          "RFormat",
        },
      }
      require("r").setup(opts)
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    dependencies = { "R-nvim/cmp-r" },
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        sources = {
          { name = "cmp_r" },
        },
        mapping = {
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ["<C-p>"] = cmp.mapping.select_prev_item(),
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
