-- ═══════════════════════════════════════════════════════════════════════════════
-- Neovim Configuration - Modular Structure
-- ═══════════════════════════════════════════════════════════════════════════════

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
