-- vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")
vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("lazyvim_user_markdown", { clear = true }),
  pattern = { "markdown" },
  callback = function(event)
    vim.opt_local.wrap = true

    local buf = event.buf
    local win = vim.fn.bufwinid(buf)
    if win ~= -1 then
      local cfg = vim.api.nvim_win_get_config(win)
      if cfg.relative ~= "" then
        vim.opt_local.spell = false
      end
    end
  end,
})
