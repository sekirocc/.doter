return {
  {
    "b3nj5m1n/kommentary",
    keys = {
      { "gcc", mode = "n" },
      { "gc", mode = "v" },
      { "gci", mode = "n" },
      { "<A-/>", "<Plug>kommentary_line_default", mode = "n", desc = "Comment Line" },
      { "<A-/>", "<Plug>kommentary_visual_default", mode = "v", desc = "Comment Selection" },
      { "<A-/>", "<Plug>kommentary_line_default", mode = "i", desc = "Comment Line" },
    },
    config = function()
      local kommentary_config = require('kommentary.config')
      kommentary_config.configure_language("c", { prefer_single_line_comments = true, })
      kommentary_config.configure_language("cpp", { prefer_single_line_comments = true, })
    end,
  },
}