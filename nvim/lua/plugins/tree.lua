return {
	{
		"nvim-tree/nvim-tree.lua",
		config = function()
			require("nvim-tree").setup({
				view = {
					width = 25,
					preserve_window_proportions = true,
				},
				git = { ignore = false },
				filters = { dotfiles = false, git_ignored = false },
				actions = {
					open_file = {
						quit_on_open = false,
						resize_window = true,
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
