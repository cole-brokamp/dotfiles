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
        nvimpager = "split_h",
        rmd_environment = "new.env()",
        min_editor_width = 82,
        buffer_opts = "winfixwidth winfixheight", --don't not list the buffer
        rconsole_width = 130,
        set_params = "no",
        debug = false,
        debug_jump = false,
        disable_cmds = {
          "RSPlot",
          "RHelp",
          "RInsert",
          "RObjBrowser",
          "RObjBrowserEnv",
          "RObjBrowserSearch",
          "RObjBrowserRefresh",
          "RClearConsole",
          "RSaveClose",
          "RCustomStart",
          "RSourceDir",
          "RFormat",
          "RMapsDesc",
          "RConfigShow",
        },
        objbr_auto_start = false,
        objbr_opendf = false,
        objbr_openlist = false,
        hook = {
          on_filetype = function()
            -- helpers -------------------------------------------------
            local Rs = require("r.send")
            local Rrun = require("r.run")
            local R = require("r")
            local M = {}
            -- stylua: ignore start
            M.line_step = function() Rs.line("move") end
            M.line_stay = function() Rs.line("stay") end
            M.par_step = function() Rs.paragraph(true) end
            M.gfx_open = function() Rs.cmd('if (!names(dev.cur()) == "unigd") httpgd::hgd(); httpgd::hgd_browse()') end
            M.gfx_close = function() Rs.cmd("httpgd::hgd_close()") end
            M.dev_load = function() Rs.cmd("devtools::load_all()") end
            M.dev_doc = function() Rs.cmd("devtools::document()") end
            M.dev_check = function() Rs.cmd("devtools::check()") end
            M.dev_manual = function() Rs.cmd("devtools::build_manual()") end
            M.dev_readme = function() Rs.cmd("devtools::build_readme()") end
            M.dev_site = function() Rs.cmd("devtools::build_site()") end
            M.dev_examples = function() Rs.cmd("devtools::run_examples()") end
            M.dev_test = function() Rs.cmd("devtools::test()") end
            M.rextendr_doc = function() Rs.cmd("rextendr::document()") end
            M.help_text = function() Rrun.action("help") end
            M.help_html = function() vim.cmd(("RSend help(%s, help_type='html')"):format(vim.fn.expand("<cword>"))) end
            M.obj_print = function() Rrun.action("print") end
            M.obj_glimpse = function() Rrun.action("tibble::glimpse") end
            -- stylua: ignore end
            local wk = require("which-key")
            wk.register({
              ["<C-Enter>"] = { M.line_step, "line step" },
              ["<localleader>,"] = { M.line_stay, "line stay" },
              ["<localleader>e"] = { M.par_step, "paragraph step" },

              ["<localleader>d"] = { group = "devtools" },
              ["<localleader>dR"] = { M.rextendr_doc, "rextendr doc" },
              ["<localleader>dc"] = { M.dev_check, "check" },
              ["<localleader>dd"] = { M.dev_doc, "document" },
              ["<localleader>de"] = { M.dev_examples, "run examples" },
              ["<localleader>dl"] = { M.dev_load, "load all" },
              ["<localleader>dm"] = { M.dev_manual, "manual" },
              ["<localleader>dr"] = { M.dev_readme, "readme" },
              ["<localleader>ds"] = { M.dev_site, "site" },
              ["<localleader>dt"] = { M.dev_test, "test" },

              ["<localleader>g"] = { group = "graphics" },
              ["<localleader>gg"] = { M.gfx_open, "open graphics" },
              ["<localleader>gx"] = { M.gfx_close, "close graphics" },

              ["<localleader>h"] = { M.help_text, "help" },
              ["<localleader>H"] = { M.help_html, "help html" },

              ["<localleader>o"] = { M.obj_print, "print object" },
              ["<localleader>O"] = { M.obj_glimpse, "glimpse" },

              ["<localleader>s"] = { group = "session" },
              ["<localleader>ss"] = { "<Cmd>RStart<CR>", "start R session" },
            }, { buffer = 0 })
          end,
        },
      }
      require("r").setup(opts)

      -- local function strip_rnvim(buf)
      --   -- remove any <Plug>R* maps, buffer and global
      --   local modes = { "n", "v", "x", "i", "s", "o", "c", "t" }
      --   for _, md in ipairs(modes) do
      --     for _, m in ipairs(vim.api.nvim_buf_get_keymap(buf, md)) do
      --       if m.rhs and m.rhs:find("<Plug>R") then
      --         pcall(vim.keymap.del, md, m.lhs, { buffer = buf })
      --       end
      --     end
      --     for _, m in ipairs(vim.api.nvim_get_keymap(md)) do
      --       if m.rhs and m.rhs:find("<Plug>R") then
      --         pcall(vim.keymap.del, md, m.lhs)
      --       end
      --     end
      --   end
      -- end

      -- vim.api.nvim_create_autocmd("FileType", {
      --   pattern = { "r", "rmd", "quarto" },
      --   callback = function(args)
      --     strip_rnvim(args.buf)
      --   end,
      -- })
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
