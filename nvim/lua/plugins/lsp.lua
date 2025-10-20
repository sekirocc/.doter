return {
  {
    "williamboman/mason.nvim",
    event = { "BufReadPre", "BufNewFile" },
    build = ":MasonUpdate",
    dependencies = {
      "neovim/nvim-lspconfig", -- Still needed for typescript-tools and clangd-extensions
      "williamboman/mason-lspconfig.nvim",
      "p00f/clangd_extensions.nvim",
      "lukas-reineke/lsp-format.nvim",
    },
    config = function()
      require("mason").setup({
        ui = {
          border = "rounded",
        },
      })
      require("config.lsp").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = { "williamboman/mason.nvim" },
  },
  {
    "p00f/clangd_extensions.nvim",
    lazy = true,
  },
  {
    "lukas-reineke/lsp-format.nvim",
    lazy = true,
  },
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
    config = function()
      -- This is configured in config/lsp/init.lua
    end,
  },
}