return {

  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    config = function()
      -- Setup orgmode
      require("orgmode").setup({
        org_agenda_files = "~/icloud/notes/*",
        org_default_notes_file = "~/icloud/notes/_todo.org",
        -- org_todo_keywords = {'TODO', 'WISHING', 'WAITING', '|', 'DONE'},
        -- win_split_mode = 'auto',
        -- win_split_mode = 'float',
        -- win_border = 'rounded',
        --      org_todo_keyword_faces = {
        --        WAITING = ':foreground blue',
        -- WISHING = ':foreground yellow',
        -- TODO = ':foreground red',
        -- DONE = ':foreground green',
        --      },
        -- org_hide_leading_stars = true,
      })
    end,
  },
}
