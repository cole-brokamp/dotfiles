
return{

-- vim.api.nvim_create_autocmd("TermOpen", {
--   callback = function()
--     vim.treesitter.stop()
--   end,
-- })

-- -- remap Esc to leave terminal mode for normal mode
-- vim.keymap.set('t', '<Esc>', [[<C-\><C-n>]], { noremap = true, silent = true })
--
--
-- function open_terminal_below()
--   vim.cmd("belowright 12split")
--   vim.cmd("terminal")
--   vim.cmd("startinsert")
-- end
--
-- function command()
--   vim.ui.input({ prompt = " > " }, function(input)
--     local current_win = vim.api.nvim_get_current_win()
--     vim.cmd("belowright 12split")
--     local new_win = vim.api.nvim_get_current_win()
--     local buf = vim.api.nvim_create_buf(false, true)
--     vim.api.nvim_win_set_buf(new_win, buf)
--     vim.fn.termopen({ "sh", "-c", input .. "; exec " .. os.getenv("SHELL") })
--     vim.api.nvim_set_current_win(current_win)
--   end)
-- end
--
-- local wk = require("which-key")
-- wk.add({
--   { "<leader>t",  open_terminal_below, desc = "terminal"},
--   { "<leader>c",  command, desc = "command"},
-- })


}
