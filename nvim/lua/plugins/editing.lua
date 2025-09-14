return {
  -- Fast fold
  {
    "Konfekt/FastFold",
    event = "BufReadPost",
  },
  {
    "tmhedberg/SimpylFold",
    ft = "python",
  },

  -- Auto pairs
  {
    "jiangmiao/auto-pairs",
    event = "InsertEnter",
  },

  -- Multiple cursors
  {
    "terryma/vim-multiple-cursors",
    keys = {
      "<C-n>",
      "<C-p>",
      "<C-x>",
    },
  },

  -- Easy motion
  {
    "easymotion/vim-easymotion",
    keys = {
      "<leader><leader>",
    },
  },

  -- Surround
  {
    "tpope/vim-surround",
    keys = {
      "cs",
      "ds",
      "ys",
      { "S", mode = "v" },
    },
  },

  -- Repeat
  {
    "tpope/vim-repeat",
    keys = ".",
  },

  -- Prettier
  {
    "prettier/vim-prettier",
    build = "npm install",
    keys = {
      { "<leader>R", ":<Plug>(Prettier):retab", desc = "Format with Prettier" },
    },
    ft = {
      "javascript",
      "typescript",
      "css",
      "scss",
      "json",
      "graphql",
      "markdown",
      "vue",
      "yaml",
      "html"
    },
  },

  -- Buffer management
  {
    "schickling/vim-bufonly",
    cmd = "BufOnly",
  },
  {
    "rbgrouleff/bclose.vim",
    cmd = "Bclose",
  },
}