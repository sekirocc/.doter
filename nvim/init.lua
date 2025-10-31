-- ═══════════════════════════════════════════════════════════════════════════════
-- Neovim Configuration - Modular Structure
-- ═══════════════════════════════════════════════════════════════════════════════

-- Set leader key BEFORE loading lazy.nvim
vim.g.mapleader = " "

-- ⚠️ 禁用 ts_ls - 必须在加载任何插件之前
-- 防止 nvim-lspconfig 或 mason 自动启动 ts_ls
-- 因为我们使用 typescript-tools.nvim 替代
vim.g.lsp_skip_servers = { 'ts_ls', 'tsserver' }

-- Bootstrap and setup lazy.nvim
require('config.lazy').setup()

-- Load core configuration modules
require('config.options').setup()     -- Vim options and global settings
require('config.functions').setup()   -- Custom functions and utilities
require('config.highlights')          -- Color scheme and highlights
require('config.autocmds').setup()    -- Autocommands
require('config.keymaps').setup()     -- Key mappings
require('config.proxy').setup()       -- SetProxy

-- Load LSP configuration
require('config.lsp').setup()         -- Language Server Protocol setup

-- Load plugin configurations are now handled by lazy.nvim
-- Individual plugin setups are in their respective files under lua/plugins/

-- print("✓ Neovim configuration loaded successfully!")
