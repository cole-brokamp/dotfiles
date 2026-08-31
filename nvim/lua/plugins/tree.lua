return {
	{
		"nvim-tree/nvim-tree.lua",
		config = function()
			require("nvim-tree").setup({
				view = {
					width = 36,
					float = {
						enable = true,
						quit_on_focus_loss = true,
						open_win_config = function()
							return {
								relative = "editor",
								border = "rounded",
								width = math.min(36, math.max(1, vim.o.columns - 4)),
								height = math.min(50, math.max(1, vim.o.lines - 4)),
								row = 1,
								col = 1,
							}
						end,
					},
				},
				git = { ignore = false },
				filters = { dotfiles = false, git_ignored = false },
				actions = {
					open_file = {
						quit_on_open = true,
						resize_window = false,
						window_picker = { enable = false },
					},
				},
			})
			local wk = require("which-key")
			wk.add({
				{ "<leader>d", "<cmd>NvimTreeFindFile<cr>", desc = "dir" },
			})
		end,
	},
}
