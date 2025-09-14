return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    dependencies = {
      "nvim-treesitter/playground",
      "Badhi/nvim-treesitter-cpp-tools",
    },
    config = function()
      require'nvim-treesitter.configs'.setup {
        ensure_installed = { "c", "cpp", "lua", "vim", "vimdoc", "query" },
        highlight = {
          enable = true
        }
      }
    end,
  },
  {
    "nvim-treesitter/playground",
    lazy = true,
  },
  {
    "Badhi/nvim-treesitter-cpp-tools",
    lazy = true,
  },
}