function SendLineToTerminal()
  -- Get the current line under the cursor
  local line = vim.api.nvim_get_current_line()

  -- Find the first terminal buffer
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.bo[buf].buftype == "terminal" then
      -- Get the terminal's channel ID
      local chan_id = vim.b[buf].terminal_job_id
      if chan_id then
        -- Send the line + newline
        vim.fn.chansend(chan_id, line .. "\n")
        print("✅ Sent to terminal: " .. line)
        return
      end
    end
  end

  print("❌ No terminal buffer found.")
end
vim.keymap.set("n", "<leader>e", SendLineToTerminal, { desc = "Eval line in terminal" })


vim.api.nvim_create_autocmd("TermOpen", {
  callback = function()
    vim.treesitter.stop()
  end,
})

-- remap Esc to leave terminal mode for normal mode
vim.keymap.set('t', '<Esc>', [[<C-\><C-n>]], { noremap = true, silent = true })

function open_terminal_below()
  vim.cmd("belowright 12split")
  vim.cmd("terminal")
  vim.cmd("startinsert")
end
vim.keymap.set("n", "<leader>t", open_terminal_below, { desc = "terminal" })

function command()
  vim.ui.input({ prompt = " > " }, function(input)
    local current_win = vim.api.nvim_get_current_win()
    vim.cmd("belowright 12split")
    local new_win = vim.api.nvim_get_current_win()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(new_win, buf)
    vim.fn.termopen({ "sh", "-c", input .. "; exec " .. os.getenv("SHELL") })
    vim.api.nvim_set_current_win(current_win)
  end)
end
vim.keymap.set("n", "<leader>c", command, { desc = "command" })

vim.keymap.set("i", "<C-'>", [[|><CR>]], { desc = "insert |> and newline" })
