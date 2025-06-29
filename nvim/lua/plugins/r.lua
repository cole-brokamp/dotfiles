return {
  {
    "R-nvim/R.nvim",
    ft = { "r", "rmd", "quarto" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      vim.g.R_assign = 2                      -- use `_ <- _` instead of `_ = _`
      vim.g.R_auto_start = 1                  -- start R automatically on opening R file
      vim.g.R_app = "Nvim-R"                  -- default: tries to detect R.app or terminal
      vim.g.R_hl_term = 0                     -- disable term highlighting
      vim.g.R_args = { "--no-save", "--quiet" }
      vim.g.R_bracketed_paste = true          -- enable bracketed paste mode
      vim.g.R_rconsole_height = 10            -- height of R console window
      vim.g.R_set_omnifunc = 1                -- use omnifunc completion
      vim.g.R_enable_mappings = 1             -- enable default keymaps

      -- Optional: set treesitter folding for R files
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "r", "rmd", "quarto" },
        callback = function()
          vim.wo.foldmethod = "expr"
          vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
        end,
      })
    end,
  },

  {
    "R-nvim/cmp-r",
    {
        "hrsh7th/nvim-cmp",
        config = function()
            require("cmp").setup({ sources = {{ name = "cmp_r" }}})
            require("cmp_r").setup({})
        end,
    },
  },
}
