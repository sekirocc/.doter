local M = {}

function M.setup()
  -- 第一步：调用插件本身的 setup（仅用于插件选项，如 API key、模型等）
  require("claudecode").setup({
    -- 可选：根据插件文档添加配置项
    -- 例如：
    -- api_key = os.getenv("CLAUDE_API_KEY"),
    -- default_model = "claude-3-sonnet-20240229",
    -- 等等...
  })

  -- 第二步：手动设置快捷键
  local map = vim.keymap.set
  local opts = { silent = true, noremap = true, desc = true }

  -- 创建父前缀组（可选，用于 which-key 提示）
  map('n', '<leader>a', '<cmd>echo "AI/Claude Code"<cr>', vim.tbl_extend("force", opts, { desc = "AI/Claude Code" }))

  -- 子命令
  map('n', '<leader>ac', '<cmd>ClaudeCode<cr>',          vim.tbl_extend("force", opts, { desc = "Toggle Claude" }))
  map('n', '<leader>af', '<cmd>ClaudeCodeFocus<cr>',     vim.tbl_extend("force", opts, { desc = "Focus Claude" }))
  map('n', '<leader>ar', '<cmd>ClaudeCode --resume<cr>', vim.tbl_extend("force", opts, { desc = "Resume Claude" }))
  map('n', '<leader>aC', '<cmd>ClaudeCode --continue<cr>', vim.tbl_extend("force", opts, { desc = "Continue Claude" }))
  map('n', '<leader>am', '<cmd>ClaudeCodeSelectModel<cr>', vim.tbl_extend("force", opts, { desc = "Select Claude model" }))
  map('n', '<leader>ab', '<cmd>ClaudeCodeAdd %<cr>',     vim.tbl_extend("force", opts, { desc = "Add current buffer" }))
  map('v', '<leader>as', '<cmd>ClaudeCodeSend<cr>',      vim.tbl_extend("force", opts, { desc = "Send to Claude" }))

  -- Diff 管理
  map('n', '<leader>aa', '<cmd>ClaudeCodeDiffAccept<cr>', vim.tbl_extend("force", opts, { desc = "Accept diff" }))
  map('n', '<leader>ad', '<cmd>ClaudeCodeDiffDeny<cr>',   vim.tbl_extend("force", opts, { desc = "Deny diff" }))

  -- 特殊上下文：在文件管理器中使用 <leader>as 添加文件
  -- 注意：Packer 不支持 ft 条件，需用 autocmd 或依赖插件自身支持
  -- 如果 claudecode.nvim 已自动处理 NvimTree 等，则无需额外绑定
  -- 否则可以加 autocmd，但建议先测试
end

return M
