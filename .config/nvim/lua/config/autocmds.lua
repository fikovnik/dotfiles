local function augroup(name)
  return vim.api.nvim_create_augroup("my_" .. name, { clear = true })
end

local function mkdir_for_current_file()
  local dir = vim.fn.expand("<afile>:p:h")

  if dir:find("%l+://") == 1 then
    return
  end

  if vim.fn.isdirectory(dir) == 0 then
    vim.fn.mkdir(dir, "p")
  end
end

vim.api.nvim_create_autocmd("FileType", {
  group = augroup("close_with_q"),
  pattern = {
    "git",
    "notify",
    "dirbuf",
    "dap-float",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
  end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
  group = augroup("create_missing_dirs"),
  pattern = "*",
  callback = mkdir_for_current_file,
})
