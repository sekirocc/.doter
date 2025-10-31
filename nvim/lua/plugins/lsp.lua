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
    dependencies = {
      "nvim-lua/plenary.nvim",
      "neovim/nvim-lspconfig",
    },
    ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
    config = function()
      -- 获取 custom_attach 函数
      local lsp_module = require("config.lsp")
      local custom_attach = lsp_module.get_custom_attach()

      require("typescript-tools").setup {
        on_attach = custom_attach,
        settings = {
          -- Prefer implementation over declaration
          preferences = {
            includeCompletionsForModuleExports = true,
            includeCompletionsWithInsertText = true,
          },
        },
      }
    end,
  },
}