--Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  { import = "plugins.buffer" },
  { import = "plugins.file" },
  { import = "plugins.edit" },
  { import = "plugins.git" },
  { import = "plugins.lsp" },
  { import = "plugins.org" },
  { import = "plugins.treesitter" },
  { import = "plugins.conform" },
  { import = "plugins.r" },
  { import = "plugins.ui" },
  { import = "plugins.which-key" },
})
