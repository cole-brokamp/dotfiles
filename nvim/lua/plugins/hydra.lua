return {
	{
		"nvimtools/hydra.nvim",
		config = function()
			local Hydra = require("hydra")

			local function send_to_terminal(text, opts)
				opts = opts or {}

				return function()
					local send = rawget(_G, "ColeSend")
					local method = opts.raw and "send_raw" or "send"

					if not send or type(send[method]) ~= "function" then
						vim.notify("ColeSend is unavailable", vim.log.levels.ERROR, { title = "R Hydra" })
						return
					end

					send[method](text)
				end
			end

			_G.hydra_window_rearrange = Hydra({
				name = "window rearrange",
				mode = "n",
				config = {
					exit = false,
					foreign_keys = nil,
				},
				heads = {
					{ ">", "<cmd>vertical resize +2<cr>" },
					{ "<", "<cmd>vertical resize -2<cr>" },
					{ "+", "<cmd>resize +2<cr>" },
					{ "-", "<cmd>resize -2<cr>" },
					{ "=", "<cmd>wincmd =<cr>" },
					{ "<Left>", "<cmd>wincmd r<cr>" },
					{ "<Right>", "<cmd>wincmd R<cr>" },
					{ "<Esc>", nil, { exit = true } },
				},
			})

			local r_command_specs = {
				{
					body = "<localleader>d",
					name = "devtools",
					heads = {
						{ key = "d", desc = "document", command = "devtools::document()" },
						{ key = "t", desc = "test", command = "devtools::test()" },
						{ key = "c", desc = "check", command = "devtools::check()" },
						{ key = "l", desc = "load all", command = "devtools::load_all()" },
					},
				},
				{
					body = "<localleader>s",
					name = "session",
					heads = {
						{ key = "c", desc = "colorize", command = "library(colorout)" },
						{ key = "g", desc = "start graphics", command = "httpgd::hgd()" },
						{ key = "i", desc = "interrupt", command = "\003", raw = true },
						{ key = "q", desc = "quit", command = "q()" },
						{ key = "Q", desc = "quit debug", command = "Q" },
						{ key = "r", desc = "restart", command = "q('no')\nR" },
					},
				},
			}

			local function create_r_mappings(bufnr)
				if vim.b[bufnr].cole_r_commands_initialized then
					return
				end

				vim.b[bufnr].cole_r_commands_initialized = true

				for _, spec in ipairs(r_command_specs) do
					for _, head in ipairs(spec.heads) do
						vim.keymap.set("n", spec.body .. head.key, send_to_terminal(head.command, { raw = head.raw }), {
							buffer = bufnr,
							desc = head.desc,
						})
					end
				end
			end

			vim.keymap.set("n", "<leader>wr", function()
				hydra_window_rearrange:activate()
			end, { desc = "rearrange" })

			vim.api.nvim_create_autocmd("FileType", {
				pattern = { "r" },
				callback = function(args)
					create_r_mappings(args.buf)
				end,
			})

			for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
				if vim.api.nvim_buf_is_valid(bufnr) and vim.bo[bufnr].filetype == "r" then
					create_r_mappings(bufnr)
				end
			end
		end,
	},
}
