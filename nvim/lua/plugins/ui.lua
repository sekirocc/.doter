return {
  -- Status line
  {
    "itchyny/lightline.vim",
    event = "VimEnter",
    dependencies = {
      "mengelbrecht/lightline-bufferline",
    },
    config = function()
      -- Lightline configuration would go here
    end,
  },
  {
    "mengelbrecht/lightline-bufferline",
    lazy = true,
  },

  -- Which key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    config = function()
      require("which-key").setup({
        triggers_blacklist = {
          -- 排除 Claude Code 的快捷键
          n = { "<C-,>" },
          i = { "<C-,>" },
          v = { "<C-,>" },
          x = { "<C-,>" },
        },
      })
    end,
  },
}