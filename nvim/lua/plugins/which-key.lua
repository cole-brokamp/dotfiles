return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		},
		keys = {
			{
				"<leader>?",
				function()
					require("which-key").show({ global = false })
				end,
				desc = "buffer local keymaps (which-key)",
			},
		},
		config = function()
			local wk = require("which-key")
			local function add_r_mappings(bufnr)
				wk.add({
					{ "<localleader>", group = "R", icon = { cat = "filetype", name = "r" }, buffer = bufnr },
					{ "<localleader>e", desc = "evaluate paragraph", buffer = bufnr },
					{ "<localleader>h", desc = "hover", buffer = bufnr },
					{ "<localleader>o", desc = "evaluate object", buffer = bufnr },
					{ "<localleader>,", desc = "evaluate line", buffer = bufnr },
					{
						"<localleader>d",
						group = "devtools",
						icon = { icon = "󰯨 ", color = "orange" },
						buffer = bufnr,
					},
					{ "<localleader>dd", desc = "document", buffer = bufnr },
					{ "<localleader>dc", desc = "check", buffer = bufnr },
					{ "<localleader>dl", desc = "load all", buffer = bufnr },
					{ "<localleader>dt", desc = "test", buffer = bufnr },
					{
						"<localleader>s",
						group = "session",
						icon = { icon = "󰆍 ", color = "azure" },
						buffer = bufnr,
					},
					{ "<localleader>sc", desc = "colorize", buffer = bufnr },
					{ "<localleader>sg", desc = "start graphics", buffer = bufnr },
					{ "<localleader>si", desc = "interrupt", buffer = bufnr },
					{ "<localleader>sq", desc = "quit", buffer = bufnr },
					{ "<localleader>sQ", desc = "quit debug", buffer = bufnr },
					{ "<localleader>sr", desc = "restart", buffer = bufnr },
				})
			end

			wk.add({
				{ "<leader>q", group = "quit" },
				{ "<leader>qq", "<cmd>qa<cr>", desc = "quit" },
				{ "<leader>q!", "<cmd>qa!<cr>", desc = "quit without saving" },
			})

			wk.add({
				{ "<leader>b", group = "buffers" },
				{ "<leader>bb", "<cmd>Telescope buffers<CR>", desc = "buffers" },
				{ "<leader>bd", "<cmd>bp | bd #<CR>", desc = "delete" },
				{ "<leader>bD", "<cmd>bd!<CR>", desc = "kill" },
				{ "<leader>br", "<cmd>e!<CR>", desc = "reload" },
				{ "<leader>bn", "<cmd>enew<CR>", desc = "new" },
				{ "<leader>bs", "<cmd>new | setlocal buftype=nofile bufhidden=hide noswapfile<CR>", desc = "scratch" },
				{ "<leader>bn", "<cmd>bn<CR>", desc = "next" },
				{ "<leader>bp", "<cmd>bp<CR>", desc = "previous" },
				{ "<leader><Tab>", "<cmd>b#<CR>", desc = "last buffer" },
			})

			wk.add({
				{ "<leader>f", group = "files" },
				{ "<leader>fs", "<cmd>write<cr>", desc = "save" },
				{ "<leader>fS", "<cmd>wall<cr>", desc = "save all" },
			})

			wk.add({
				{ "<leader>w", group = "window" },
				{ "<leader>wh", "<C-w>h", desc = "go left" },
				{ "<leader>wj", "<C-w>j", desc = "go down" },
				{ "<leader>wk", "<C-w>k", desc = "go up" },
				{ "<leader>wl", "<C-w>l", desc = "go right" },
				{ "<leader>w/", "<C-w>v", desc = "split right" },
				{ "<leader>w-", "<C-w>s", desc = "split down" },
				{ "<leader>wH", "<C-w>H", desc = "move far left" },
				{ "<leader>wJ", "<C-w>J", desc = "move to bottom`" },
				{ "<leader>wK", "<C-w>K", desc = "move to top" },
				{ "<leader>wL", "<C-w>L", desc = "move to far right" },
				{ "<leader>wd", "<C-w>q", desc = "delete window" },
				{ "<leader>ws", "<C-w>x", desc = "swap windows" },
			})

			vim.api.nvim_create_autocmd("FileType", {
				pattern = { "r" },
				callback = function(args)
					add_r_mappings(args.buf)
				end,
			})

			for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
				if vim.api.nvim_buf_is_valid(bufnr) and vim.bo[bufnr].filetype == "r" then
					add_r_mappings(bufnr)
				end
			end
		end,
	},
}
