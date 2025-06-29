local function setup_lsp(opts)
  opts.on_attach = on_attach
  vim.lsp.start(opts)
end

require("lsp.air")(setup_lsp)
