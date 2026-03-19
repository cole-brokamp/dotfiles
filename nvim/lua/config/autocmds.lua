local Send = {}

local send_state = {
  term_buf = nil,
}

local function notify(msg, level)
  vim.notify(msg, level or vim.log.levels.INFO, { title = "Send" })
end

local function get_term_job(bufnr)
  if not bufnr or not vim.api.nvim_buf_is_valid(bufnr) then
    return nil
  end

  if vim.bo[bufnr].buftype ~= "terminal" then
    return nil
  end

  local ok, chan = pcall(vim.api.nvim_buf_get_var, bufnr, "terminal_job_id")
  if ok then
    return chan
  end

  return vim.b[bufnr].terminal_job_id
end

local function is_tracked_terminal_valid()
  return get_term_job(send_state.term_buf) ~= nil
end

local function clear_tracked_terminal_if_needed(bufnr)
  if send_state.term_buf == bufnr then
    send_state.term_buf = nil
  end
end

local function find_terminal_window(bufnr)
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_buf(win) == bufnr then
      return win
    end
  end

  return nil
end

local function ensure_terminal(opts)
  opts = opts or {}

  if is_tracked_terminal_valid() then
    return send_state.term_buf
  end

  local current_win = vim.api.nvim_get_current_win()
  local height = opts.height or 16

  vim.cmd(("belowright %dsplit"):format(height))
  vim.cmd("terminal")

  local term_buf = vim.api.nvim_get_current_buf()
  send_state.term_buf = term_buf

  vim.api.nvim_create_autocmd({ "BufWipeout", "TermClose" }, {
    buffer = term_buf,
    once = true,
    callback = function(args)
      clear_tracked_terminal_if_needed(args.buf)
    end,
  })

  if not opts.focus then
    vim.api.nvim_set_current_win(current_win)
  else
    vim.cmd("startinsert")
  end

  return term_buf
end

local function open_untracked_terminal(opts)
  opts = opts or {}

  vim.cmd(("belowright %dsplit"):format(opts.height or 16))
  vim.cmd("terminal")
  vim.cmd("startinsert")

  return vim.api.nvim_get_current_buf()
end

local function send_text(text)
  if not text or text == "" then
    return
  end

  local term_buf = ensure_terminal()
  local chan_id = get_term_job(term_buf)
  if not chan_id then
    notify("No terminal buffer available", vim.log.levels.ERROR)
    return
  end

  if not text:match("\n$") then
    text = text .. "\n"
  end

  vim.fn.chansend(chan_id, text)
end

function Send.open_terminal()
  local term_buf = ensure_terminal({ focus = true, height = 16 })
  local win = find_terminal_window(term_buf)
  if win then
    vim.api.nvim_set_current_win(win)
    vim.cmd("startinsert")
  end
end

function Send.open_scratch_terminal()
  open_untracked_terminal({ height = 16 })
end

function Send.focus_terminal()
  local term_buf = ensure_terminal({ focus = false, height = 16 })
  local win = find_terminal_window(term_buf)
  if win then
    vim.api.nvim_set_current_win(win)
    vim.cmd("startinsert")
  end
end

function Send.send_line()
  local bufnr = 0
  local row, _ = unpack(vim.api.nvim_win_get_cursor(0))
  local last_row = vim.api.nvim_buf_line_count(bufnr)
  local line = vim.api.nvim_get_current_line()
  send_text(line)

  if row < last_row then
    vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  end
end

function Send.send_word()
  local word = vim.fn.expand("<cword>")
  send_text(word)
end

function Send.send_paragraph()
  local bufnr = 0
  local cur_line = vim.api.nvim_win_get_cursor(0)[1]
  local total_lines = vim.api.nvim_buf_line_count(bufnr)

  local function is_blank(line)
    return vim.trim(line) == ""
  end

  local start_line = cur_line
  while start_line > 1 do
    local line = vim.api.nvim_buf_get_lines(bufnr, start_line - 2, start_line - 1, false)[1]
    if is_blank(line) then
      break
    end
    start_line = start_line - 1
  end

  local end_line = cur_line
  while end_line < total_lines do
    local line = vim.api.nvim_buf_get_lines(bufnr, end_line, end_line + 1, false)[1]
    if is_blank(line) then
      break
    end
    end_line = end_line + 1
  end

  local lines = vim.api.nvim_buf_get_lines(bufnr, start_line - 1, end_line, false)
  send_text(table.concat(lines, "\n"))

  local next_line = end_line + 1
  while next_line <= total_lines do
    local line = vim.api.nvim_buf_get_lines(bufnr, next_line - 1, next_line, false)[1]
    if vim.trim(line) ~= "" then
      vim.api.nvim_win_set_cursor(0, { next_line, 0 })
      return
    end
    next_line = next_line + 1
  end
end

function Send.send_selection()
  local bufnr = 0
  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")

  local start_row = start_pos[2]
  local start_col = start_pos[3]
  local end_row = end_pos[2]
  local end_col = end_pos[3]

  if start_row == 0 or end_row == 0 then
    return
  end

  if start_row > end_row or (start_row == end_row and start_col > end_col) then
    start_row, end_row = end_row, start_row
    start_col, end_col = end_col, start_col
  end

  local lines = vim.api.nvim_buf_get_lines(bufnr, start_row - 1, end_row, false)
  if #lines == 0 then
    return
  end

  lines[1] = string.sub(lines[1], start_col)
  lines[#lines] = string.sub(lines[#lines], 1, end_col)

  send_text(table.concat(lines, "\n"))
end

function Send.prompt_and_send()
  vim.ui.input({ prompt = "Send to terminal: " }, function(input)
    if not input or vim.trim(input) == "" then
      return
    end
    send_text(input)
  end)
end

_G.ColeSend = Send

-- query search and replace
function query_replace()
  vim.ui.input({ prompt = "Search for: " }, function(search)
    if not search or search == "" then
      return
    end

    vim.ui.input({ prompt = "Replace with: " }, function(replace)
      if replace == nil then
        return
      end

      local cmd = string.format("%%s/%s/%s/gc", search, replace)
      vim.cmd(cmd)
    end)
  end)
end

vim.api.nvim_create_autocmd("TermOpen", {
  callback = function()
    vim.treesitter.stop()
  end,
})

vim.keymap.set("t", "<Esc>", [[<C-\><C-n>]], { noremap = true, silent = true })

vim.keymap.set("n", "<leader>t", Send.open_terminal, { desc = "terminal" })
vim.keymap.set("n", "<leader>T", Send.open_scratch_terminal, { desc = "scratch terminal" })
vim.keymap.set("n", "<leader>c", Send.prompt_and_send, { desc = "send command" })
vim.keymap.set("n", "<leader>el", Send.send_line, { desc = "send line" })
vim.keymap.set("n", "<leader>ee", Send.send_paragraph, { desc = "evaluate paragraph" })
vim.keymap.set("n", "<leader>eo", Send.send_word, { desc = "evaluate object" })
vim.keymap.set("x", "<leader>es", function()
  vim.schedule(Send.send_selection)
end, { desc = "evaluate selection" })

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "r" },
  callback = function(args)
    local opts = { buffer = args.buf }
    vim.keymap.set("n", "<localleader>e", Send.send_paragraph, opts)
    vim.keymap.set("n", "<localleader>,", Send.send_line, opts)
    vim.keymap.set("n", "<localleader>o", Send.send_word, opts)
  end,
})

vim.keymap.set("i", "<C-'>", [[ |><CR>]], { desc = "insert |> and newline" })
