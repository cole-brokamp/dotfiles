return function(setup)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "r",
    callback = function()
      setup({
        name = "air",
        cmd = { "air", "language-server" },
        filetypes = { "r" },
        root_dir = vim.fs.dirname(vim.fs.find({
          "air.toml",
          ".air.toml",
          ".git",
        }, { upward = true })[1]),
      })
    end,
  })
end
