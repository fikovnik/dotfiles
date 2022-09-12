local M = {}

local fmt_group = vim.api.nvim_create_augroup('formatting', { clear = true })

function M.bind_keys(buf)
  local function map(mode, lhs, rhs, desc)
    local opts = { buffer = buf, silent = true, desc = desc }
    vim.keymap.set(mode, lhs, rhs, opts)
  end

  map('n', '<localleader>D', vim.lsp.buf.declaration, 'Declaration')
  map('n', '<localleader>d', vim.lsp.buf.definition, 'Definition')
  map('n', '<localleader>K', vim.lsp.buf.hover, 'Hover')
  map('n', '<localleader>i', vim.lsp.buf.implementation, 'Implementation')
  map('n', '<localleader>r', vim.lsp.buf.references, 'References')
  map('n', '<localleader>t', vim.lsp.buf.type_definition, 'Type')
  map('n', '<localleader>R', vim.lsp.buf.rename, 'Rename')
  map('n', '<localleader>a', vim.lsp.buf.code_action, 'Actions')
  map('n', '<localleader>f', function() vim.lsp.buf.format { async = true } end, 'Format')
  -- map('n', '<leader>so', [[<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>]], opts)

  map('v', '<localleader>a', vim.lsp.buf.range_code_action, 'Actions')
  map('v', '<localleader>f', vim.lsp.buf.range_formatting, 'Format')

  map('i', '<C-S-?>', vim.lsp.buf.signature_help, 'Signature')
  --
  -- map('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- map('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  -- map('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
end

function M.fmt_on_save(client, buf)
  if client.supports_method('textDocument/formatting') then
    vim.api.nvim_create_autocmd('BufWritePre', {
      group = fmt_group,
      buffer = buf,
      callback = function()
        vim.lsp.buf.format({
          timeout_ms = 3000,
          buffer = buf,
        })
      end,
    })
  end
end

return M
