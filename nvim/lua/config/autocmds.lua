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

function send_paragraph_to_terminal()
  local bufnr = 0
  local cur_line = vim.api.nvim_win_get_cursor(0)[1]
  local total_lines = vim.api.nvim_buf_line_count(bufnr)

  local function is_blank(line)
    return vim.trim(line) == ""
  end

  -- Expand upward
  local start_line = cur_line
  while start_line > 1 do
    local line = vim.api.nvim_buf_get_lines(bufnr, start_line - 2, start_line - 1, false)[1]
    if is_blank(line) then break end
    start_line = start_line - 1
  end

  -- Expand downward
  local end_line = cur_line
  while end_line < total_lines do
    local line = vim.api.nvim_buf_get_lines(bufnr, end_line, end_line + 1, false)[1]
    if is_blank(line) then break end
    end_line = end_line + 1
  end

  -- Get the paragraph lines
  local lines = vim.api.nvim_buf_get_lines(bufnr, start_line - 1, end_line, false)
  local text = table.concat(lines, "\n") .. "\n"

  -- Find first terminal buffer and send
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.bo[buf].buftype == "terminal" then
      local chan_id = vim.b[buf].terminal_job_id
      if chan_id then
        vim.fn.chansend(chan_id, text)
        print("✅ sent paragraph to terminal")
        return
      end
    end
  end


  print("❌ no terminal buffer found")
end
vim.keymap.set("n", "<leader>e", send_paragraph_to_terminal, { desc = "Eval paragraph in terminal" })


