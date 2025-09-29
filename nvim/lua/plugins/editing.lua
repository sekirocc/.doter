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

  -- Auto pairs - Modern Neovim-compatible version
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    dependencies = { "hrsh7th/nvim-cmp" },
    config = function()
      local autopairs = require("nvim-autopairs")
      autopairs.setup({
        check_ts = true, -- Enable treesitter integration
        ts_config = {
          lua = {'string'},-- it will not add a pair on that treesitter node
          javascript = {'template_string'},
          java = false,-- don't check treesitter on java
        },
        disable_filetype = { "TelescopePrompt", "vim" },
        fast_wrap = {
          map = '<M-e>',
          chars = { '{', '[', '(', '"', "'" },
          pattern = [=[[%'%"%)%>%]%)%}%,]]=],
          end_key = '$',
          keys = 'qwertyuiopzxcvbnmasdfghjkl',
          check_comma = true,
          highlight = 'Search',
          highlight_grey='Comment'
        },
      })

      -- Integration with nvim-cmp
      local cmp_autopairs = require('nvim-autopairs.completion.cmp')
      local cmp = require('cmp')
      cmp.event:on(
        'confirm_done',
        cmp_autopairs.on_confirm_done()
      )
    end,
  },

  -- Multiple cursors
  {
    "terryma/vim-multiple-cursors",
    keys = {
      { "<C-n>", mode = { "n", "v" } },
      { "<C-p>", mode = { "n", "v" } },
      { "<C-x>", mode = { "n", "v" } },
    },
  },

  -- Easy motion
  {
    "easymotion/vim-easymotion",
    keys = {
      { "f", "<Plug>(easymotion-bd-w)", mode = "n", desc = "EasyMotion word" },
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