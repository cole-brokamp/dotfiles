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
        user_only_maps = true,
        hook = {
          on_filetype = function()
            local function bufmap(mode, lhs, rhs, opts)
              opts = vim.tbl_extend("force", {
                noremap = true,
                silent = true,
                buffer = true,
              }, opts or {})
              vim.keymap.set(mode, lhs, rhs, opts)
            end

            local maps = {
              -- send to R terminal
              {
                { "n" },
                "<LocalLeader>,",
                "<Cmd>lua require('r.send').line('stay')<CR>",
                { desc = "> line and stay" },
              },
              { { "v" }, "<C-Enter>", "<Plug>RSendSelection", { desc = "> selection<CR>" } },
              { { "n" }, "<C-Enter>", "<Cmd>lua require('r.send').line('move')<CR>", { desc = "> line and step" } },
              {
                { "n" },
                "<LocalLeader>e",
                "<Cmd>lua require('r.send').paragraph(true)<CR>",
                { desc = "> paragraph and step" },
              },

              -- on word under cursor
              { { "n" }, "<LocalLeader>o", "<Cmd>lua require('r.run').action('print')<CR>", { desc = "object print" } },
              { { "n" }, "<LocalLeader>h", "<Cmd>lua require('r.run').action('help')<CR>", { desc = "help" } },
              {
                { "n" },
                "<LocalLeader>H",
                "<Cmd>lua require('r.run').action('help(type = 'html')')<CR>",
                { desc = "help in browser" },
              },

              {
                { "n" },
                "<LocalLeader>O",
                "<Cmd>lua require('r.run').action('tibble::glimpse')<CR>",
                { desc = "object glimpse" },
              },

              -- graphics
              {
                { "n" },
                "<LocalLeader>gg",
                "<Cmd>lua require('r.send').cmd('if (!names(dev.cur()) == \"unigd\") httpgd::hgd(); httpgd::hgd_browse()')<CR>",
                { desc = "open graphics server" },
              },
              {
                { "n" },
                "<LocalLeader>gx",
                "<Cmd>lua require('r.send').cmd('httpgd::hgd_close()')<CR>",
                { desc = "kill graphics server" },
              },

              -- devtools
              {
                { "n" },
                "<LocalLeader>dl",
                "<Cmd>lua require('r.send').cmd('devtools::load_all()')<CR>",
                { desc = "load all" },
              },
              {
                { "n" },
                "<LocalLeader>dd",
                "<Cmd>lua require('r.send').cmd('devtools::document()')<CR>",
                { desc = "document" },
              },
              {
                { "n" },
                "<LocalLeader>dc",
                "<Cmd>lua require('r.send').cmd('devtools::check()')<CR>",
                { desc = "check" },
              },
              {
                { "n" },
                "<LocalLeader>dr",
                "<Cmd>lua require('r.send').cmd('devtools::build_readme()')<CR>",
                { desc = "build readme" },
              },
              {
                { "n" },
                "<LocalLeader>de",
                "<Cmd>lua require('r.send').cmd('devtools::run_examples()')<CR>",
                { desc = "run examples" },
              },
              {
                { "n" },
                "<LocalLeader>ds",
                "<Cmd>lua require('r.send').cmd('devtools::build_site()')<CR>",
                { desc = "build site" },
              },
              {
                { "n" },
                "<LocalLeader>dm",
                "<Cmd>lua require('r.send').cmd('devtools::build_manual()')<CR>",
                { desc = "build pdf manual" },
              },
              {
                { "n" },
                "<LocalLeader>dR",
                "<Cmd>lua require('r.send').cmd('rextendr::document()')<CR>",
                { desc = "rextendr document" },
              },

              {
                { "n" },
                "<LocalLeader>tt",
                "<Cmd>lua require('r.send').cmd('devtools::test()')<CR>",
                { desc = "test package" },
              },

              -- example with Lua callback
              {
                { "n" },
                "<LocalLeader>fi",
                function()
                  local fname = vim.api.nvim_buf_get_name(0)
                  require("r.send").cmd('file.info("' .. fname .. '")')
                end,
              },
            }

            -- 3) loop and apply
            for _, m in ipairs(maps) do
              bufmap(m[1], m[2], m[3], m[4])
            end
          end,
        },
        clear_line = true,
        nvimpager = "split_h",
        rmd_environment = "new.env()",
        min_editor_width = 82,
        buffer_opts = "winfixwidth winfixheight", --don't not list the buffer
        rconsole_width = 130,
        set_params = "no",
        debug = false,
        debug_jump = false,
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
