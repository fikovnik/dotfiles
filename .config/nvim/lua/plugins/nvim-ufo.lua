if not pcall(require, 'ufo') then
  return
end

if not (vim.fn.has('nvim-0.7.0') > 0) then
  print("Warning: we require neovim 0.7.0 or higher for folding config.")
  return
end

vim.o.foldcolumn = '0'
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

require('ufo').setup {
  provider_selector = function(bufnr, filetype, buftype)
    -- Use treesitter if available
    if pcall(require, 'nvim-treesitter.parsers') then
      if require("nvim-treesitter.parsers").has_parser(filetype) then
        return { 'treesitter', 'indent' }
      end
    end

    -- Otherwise, might need to disable 'fold providers'
    -- so that we can fallback to the default vim's folding behavior (per foldmethod).
    -- This can be also helpful for a bug where all open/closed folds are lost and reset
    -- whenever fold is updated when, for instance, saving the buffer (when foldlevel != 99).
    -- For more details, see kevinhwang91/nvim-ufo#30
    return ''
  end
}

require('keybinds').bind_ufo_keys()
