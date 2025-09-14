return {
  -- Code navigation and search
  {
    "dhananjaylatkar/cscope_maps.nvim",
    ft = { "c", "cpp" },
    config = function()
      require('cscope_maps').setup({
        disable_maps = false,
        skip_input_prompt = false,
        prefix = "<leader>,",
        cscope = {
          db_file = "./.cscope.out",
          exec = "cscope",
          picker = "quickfix",
          skip_picker_for_single_result = false,
          db_build_cmd = {args = { "-bqkv" }},
          statusline_indicator = nil,
        }
      })
    end,
  },
  {
    "dyng/ctrlsf.vim",
    keys = {
      { "<leader>sf", desc = "CtrlSF Search" },
      { "<leader>st", desc = "CtrlSF Toggle" },
      { "<leader>O", "<cmd>CtrlSFOpen<cr>", desc = "CtrlSF Open" },
      { "<leader>m", "<Plug>CtrlSFCwordPath", desc = "Search Word Under Cursor" },
      { "<leader>m", "<Plug>CtrlSFVwordExec<cr>", mode = "v", desc = "Search Selection" },
    },
  },
  {
    "liuchengxu/vista.vim",
    keys = {
      { "<leader>v", "<cmd>Vista!!<cr>", desc = "Toggle Vista" },
    },
    cmd = "Vista",
  },

  -- Directory navigation
  {
    "airblade/vim-rooter",
    event = "BufReadPost",
  },
}