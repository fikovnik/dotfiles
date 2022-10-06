local present, treesitter = pcall(require, "nvim-treesitter.configs")
if not present then return end

treesitter.setup {
  ensure_installed = 'all',
  sync_install = false,
  ignore_install = { 'latex', 'help' },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<M-=>",
      node_incremental = "<M-=>",
      scope_incremental = "<M-+>",
      node_decremental = "<M-->",
    },
  },
  indent = {
    enable = false,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ["ab"] = "@block.outer",
        ["ib"] = "@block.inner",
        ["ac"] = "@call.outer",
        ["ic"] = "@call.inner",
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["aa"] = "@parameter.outer",
        ["ia"] = "@parameter.inner",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ["<M->>"] = "@parameter.inner",
      },
      swap_previous = {
        ["<M-<>"] = "@parameter.inner",
      },
    },
  },
}

-- vim.api.nvim_create_autocmd({ 'BufEnter', 'BufAdd', 'BufNew', 'BufNewFile', 'BufWinEnter' }, {
--   group = vim.api.nvim_create_augroup('TS_FOLD_WORKAROUND', {}),
--   callback = function()
--     vim.opt.foldmethod = 'expr'
--     vim.opt.foldexpr   = 'nvim_treesitter#foldexpr()'
--   end
-- })
