return {
	{
		"shortcuts/no-neck-pain.nvim",
		version = "*",
		ft = "markdown",
		config = function()
			local no_neck_pain = require("no-neck-pain")
			local markdown_width = 150

			no_neck_pain.setup({
				-- Include the two split separators so the Markdown text area is 150 columns.
				width = markdown_width + 2,
				minSideBufferWidth = 8,
				autocmds = {
					skipEnteringNoNeckPainBuffer = true,
				},
				buffers = {
					wo = {
						fillchars = "eob: ",
					},
				},
			})

			local sync_scheduled = false
			local width_scheduled = false

			local function tab_has_markdown_window()
				for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
					local config = vim.api.nvim_win_get_config(win)
					local buf = vim.api.nvim_win_get_buf(win)

					if config.relative == "" and vim.bo[buf].filetype == "markdown" then
						return true
					end
				end

				return false
			end

			local function enforce_markdown_width()
				if width_scheduled then
					return
				end

				width_scheduled = true
				vim.defer_fn(function()
					width_scheduled = false

					for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
						local buf = vim.api.nvim_win_get_buf(win)

						if vim.bo[buf].filetype == "markdown" and vim.api.nvim_win_get_width(win) > markdown_width then
							vim.api.nvim_win_set_width(win, markdown_width)
						end
					end
				end, 50)
			end

			local function is_enabled()
				return _G.NoNeckPain.state ~= nil and _G.NoNeckPain.state.enabled
			end

			local function current_tab_is_centered()
				local state = _G.NoNeckPain.state
				local tab = vim.api.nvim_get_current_tabpage()

				return state ~= nil and state.tabs ~= nil and state.tabs[tab] ~= nil
			end

			local function sync_markdown_centering()
				if sync_scheduled then
					return
				end

				sync_scheduled = true
				vim.schedule(function()
					sync_scheduled = false

					if tab_has_markdown_window() then
						if not current_tab_is_centered() then
							no_neck_pain.enable()
						end
					elseif is_enabled() and current_tab_is_centered() then
						no_neck_pain.disable()
					end
				end)
			end

			local group = vim.api.nvim_create_augroup("MarkdownCentering", { clear = true })
			vim.api.nvim_create_autocmd({
				"BufWinEnter",
				"BufWinLeave",
				"FileType",
				"TabEnter",
				"VimResized",
				"WinClosed",
				"WinNew",
				"WinResized",
			}, {
				group = group,
				callback = function()
					sync_markdown_centering()
					enforce_markdown_width()
				end,
			})

			sync_markdown_centering()
			enforce_markdown_width()
		end,
	},
}
