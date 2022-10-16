vim.cmd [[packadd packer.nvim]]

return require('packer').startup({
  function(use)
    use('wbthomason/packer.nvim')
    use('nvim-lua/plenary.nvim')
    use('lewis6991/impatient.nvim')

    use { 'rmehri01/onenord.nvim',
      config = function()
        require('plugins.theme')
      end
    }

    use { 'nvim-lualine/lualine.nvim',
      config = function()
        require('plugins.lualine')
      end
    }

    use { 'neovim/nvim-lspconfig',
      -- event = { 'BufReadPre', 'BufNewFile' },
      -- event = 'BufReadPre',
      config = function()
        require('plugins.lsp.config')
      end,
      requires = {
        'hrsh7th/cmp-nvim-lsp',
      },
    }

    use { 'hrsh7th/nvim-cmp',
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

    use { 'onsails/lspkind.nvim' }

    use { 'j-hui/fidget.nvim',
      after = { 'nvim-lspconfig' },
      config = function()
        require('fidget').setup {
          text = { spinner = 'dots' },
        }
      end,
    }

    use { 'numToStr/Comment.nvim',
      event = 'BufRead',
      config = function()
        require('Comment').setup()
      end,
    }

    use { 'williamboman/mason.nvim',
      cmd = { 'Mason', 'MasonInstall', 'MasonInstallAll', 'MasonUninstall', 'MasonUninstallAll', 'MasonLog', },
      config = function()
        require('plugins.mason')
      end,
    }

    --
    -- Treesitter
    --
    use { 'nvim-treesitter/nvim-treesitter',
      config = function() require('plugins.treesitter') end,
      run = function() require('nvim-treesitter.install').update { with_sync = true } end,
    }

    use { 'nvim-treesitter/nvim-treesitter-textobjects',
      after = 'nvim-treesitter',
    }

    use { 'nvim-treesitter/nvim-treesitter-context',
      after = 'nvim-treesitter',
    }

    use { 'kevinhwang91/nvim-ufo',
      requires = 'kevinhwang91/promise-async',
      after = 'nvim-treesitter',
      config = function() require('plugins.nvim-ufo') end,
    }

    use { 'nvim-telescope/telescope.nvim',
      event = 'CursorHold',
      config = function()
        require('plugins.telescope')
      end,
    }

    use { 'folke/which-key.nvim',
      event = 'CursorHold',
      config = function()
        require('plugins.which-key')
      end,
    }

    use { 'AckslD/nvim-neoclip.lua',
      after = 'telescope.nvim',
      config = function()
        require('neoclip').setup {
          preview = true,
        }
        require('telescope').load_extension('neoclip')
      end,
    }

    use { 'nvim-telescope/telescope-file-browser.nvim',
      after = 'telescope.nvim',
      config = function()
        require('telescope').load_extension('file_browser')
      end,
    }

    use { 'nvim-telescope/telescope-fzf-native.nvim',
      after = 'telescope.nvim',
      run = 'make',
      config = function()
        require('telescope').load_extension('fzf')
      end,
    }

    use { 'nvim-telescope/telescope-ui-select.nvim',
      after = 'telescope.nvim',
      config = function()
        require('telescope').load_extension('ui-select')
      end,
    }

    use { 'aserowy/tmux.nvim',
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

    use { 'ntpeters/vim-better-whitespace',
      event = 'BufRead',
      config = function()
        vim.g.better_whitespace_enabled = 0
        vim.g.strip_only_modified_lines = 1
        vim.g.strip_whitespace_on_save = 1
        vim.g.strip_whitespace_confirm = 0
      end,
    }

    use { 'rlane/pounce.nvim',
      event = 'CursorHold',
    }

    use { 'jinh0/eyeliner.nvim',
      config = function()
        require('eyeliner').setup {
          highlight_on_key = true
        }
      end,
    }

    use { 'tpope/vim-fugitive',
      cmd = {
        'G',
        'Git',
        'Ggrep',
        'Gdiffsplit',
        'Gvdiffsplit',
        'GBrowse',
      },
    }

    use { 'sindrets/diffview.nvim',
      config = function()
        require("diffview").setup()
      end,
      cmd = {
        'DiffviewOpen',
        'DiffviewFileHistory',
      },
    }

    use { 'lewis6991/gitsigns.nvim',
      event = 'BufRead',
      config = function()
        require('plugins.gitsigns')
      end,
    }

    use { 'simrat39/rust-tools.nvim',
      ft = 'rust',
      after = 'nvim-lspconfig',
      config = function()
        require('plugins.lsp.rust')
      end,
    }

    use { 'SmiteshP/nvim-navic',
      after = 'nvim-lspconfig',
    }

    use { 'https://gitlab.com/yorickpeterse/nvim-pqf',
      config = function()
        require('pqf').setup()
      end,
    }

    --
    -- Debugging
    --
    use { 'sakhnik/nvim-gdb',
      cmd = { 'GdbStart', },
      setup = function()
        vim.g.nvimgdb_disable_start_keymaps = 0
      end,
      config = function()
        vim.g.nvimgdb_use_cmake_to_find_executables = 0
        vim.g.nvimgdb_use_find_executables = 0
      end,
    }

    use { 'mfussenegger/nvim-dap',
      config = function()
        require('plugins.dap')
      end,
    }

    use { 'rcarriga/nvim-dap-ui',
      after = 'nvim-dap',
    }

    use { 'stevearc/aerial.nvim',
      config = function() require('plugins.aerial') end,
    }

    use { 'folke/trouble.nvim',
      after = 'telescope.nvim',
      config = function() require('trouble').setup {
          icons = false,
          use_diagnostic_signs = true,
          auto_jump = { 'lsp_definitions', 'lsp_references', 'lsp_implementations', 'lsp_type_definitions' },
        }
      end,
    }

    use {
      'folke/todo-comments.nvim',
      cmd = { 'TodoTelescope', 'TodoTrouble' },
      requires = 'nvim-lua/plenary.nvim',
      after = 'trouble.nvim',
      config = function()
        require('todo-comments').setup {}
      end,
    }

    use {
      'lukas-reineke/indent-blankline.nvim',
      event = 'BufRead',
      config = function()
        require('plugins.indentline')
      end,
    }

    use {
      'https://git.sr.ht/~whynothugo/lsp_lines.nvim',
      config = function()
        vim.diagnostic.config {
          virtual_text = false,
        }
        require('lsp_lines').setup()
      end,
    }

    use {
      'anuvyklack/windows.nvim',
      -- cmd = { 'WindowsMaximize', 'WindowsMaximizeVertically', 'WindowsMaximizeHorizontally', 'WindowsEqualize' },
      requires = 'anuvyklack/middleclass',
      config = function()
        require("windows").setup {
          autowidth = {
            enable = true,
          },
          animation = {
            enable = false,
          }
        }
      end
    }

  end
})
