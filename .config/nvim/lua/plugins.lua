vim.cmd [[packadd packer.nvim]]

return require('packer').startup({
  function(use)
    use('wbthomason/packer.nvim')
    use('nvim-lua/plenary.nvim')
    use('lewis6991/impatient.nvim')

    use {
      'rmehri01/onenord.nvim',
      config = function()
        require('plugins.theme')
      end
    }

    use {
      'nvim-lualine/lualine.nvim',
      config = function()
        require('plugins.lualine')
      end
    }

    use {
      'neovim/nvim-lspconfig',
      event = "BufReadPre",
      config = function()
        require('plugins.lsp.config')
      end,
      requires = {
        'hrsh7th/cmp-nvim-lsp',
      },
    }

    use {
      'hrsh7th/nvim-cmp',
      event = 'InsertEnter',
      config = function()
        require('plugins.lsp.nvim-cmp')
      end,
    }

    use { 'hrsh7th/cmp-path', after = 'nvim-cmp' }
    use { 'hrsh7th/cmp-buffer', after = 'nvim-cmp' }
    use { 'hrsh7th/cmp-omni', after = 'nvim-cmp' }
    use { 'kdheepak/cmp-latex-symbols', after = 'nvim-cmp' }

    use { 'dcampos/nvim-snippy' }

    use { 'dcampos/cmp-snippy', after = { 'nvim-cmp', 'nvim-snippy' } }

    use {
      'j-hui/fidget.nvim',
      after = { 'nvim-lspconfig' },
      config = function()
        require('fidget').setup {
          text = { spinner = 'dots' },
        }
      end,
    }

    use {
      'numToStr/Comment.nvim',
      event = 'BufRead',
      config = function()
        require('Comment').setup()
      end,
    }

    use {
      'williamboman/mason.nvim',
      cmd = { 'Mason', 'MasonInstall', 'MasonInstallAll', 'MasonUninstall', 'MasonUninstallAll', 'MasonLog', },
      config = function()
        require('plugins.mason')
      end,
    }

    use {
      'nvim-telescope/telescope.nvim',
      event = 'CursorHold',
      config = function()
        require('plugins.telescope')
      end,
    }

    use {
      'folke/which-key.nvim',
      event = 'CursorHold',
      config = function()
        require('plugins.which-key')
      end,
    }

    use {
      'AckslD/nvim-neoclip.lua',
      after = 'telescope.nvim',
      config = function()
        require('neoclip').setup {
          preview = true,
        }
        require('telescope').load_extension('neoclip')
      end,
    }

    use {
      'nvim-telescope/telescope-file-browser.nvim',
      after = 'telescope.nvim',
      config = function()
        require('telescope').load_extension('file_browser')
      end,
    }

    use {
      'nvim-telescope/telescope-fzf-native.nvim',
      after = 'telescope.nvim',
      run = 'make',
      config = function()
        require('telescope').load_extension('fzf')
      end,
    }

    use {
      'nvim-telescope/telescope-ui-select.nvim',
      after = 'telescope.nvim',
      config = function()
        require('telescope').load_extension('ui-select')
      end,
    }

    use {
      'aserowy/tmux.nvim',
      event = 'CursorHold',
      config = function()
        require('tmux').setup {
          navigation = {
            persist_zoom = true,
          },
        }
        require('keybinds').set_tmux_integration()
      end,
    }

    use { 'axelf4/vim-strip-trailing-whitespace' }

    use {
      'rlane/pounce.nvim',
      event = 'CursorHold',
    }

    use {
      'jinh0/eyeliner.nvim',
      config = function()
        require('eyeliner').setup {
          highlight_on_key = true
        }
      end,
    }

    use {
      'TimUntersberger/neogit',
      cmd = 'Neogit',
      config = function()
        require('neogit').setup {
          integrations = {
            diffview = true
          }
        }
      end,
    }

    use {
      'sindrets/diffview.nvim',
      after = 'neogit'
    }

    use {
      'lewis6991/gitsigns.nvim',
      event = 'BufRead',
      config = function()
        require('plugins.gitsigns')
      end,
    }

    use {
      'simrat39/rust-tools.nvim',
      ft = 'rust',
      after = 'nvim-lspconfig',
      config = function()
        require('plugins.lsp.rust')
      end,

    }

  end
})
