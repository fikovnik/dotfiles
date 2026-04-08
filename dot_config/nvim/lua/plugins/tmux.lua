local function slime_send_file()
  local ft = vim.bo.filetype
  local file = vim.fn.expand("%:p")
  if file == "" then
    vim.notify("Current buffer has no filename.", vim.log.levels.ERROR)
    return
  end

  local function slime_send(s)
    -- slime#send accepts a list of lines; ensure final newline so REPL executes
    if not s:match("\n$") then
      s = s .. "\n"
    end
    vim.fn["slime#send"]({ s })
  end

  local cmd
  if ft == "r" then
    cmd = "source('" .. file .. "')"
  elseif ft == "python" then
    cmd = 'exec(open("' .. file .. '").read())'
  elseif ft == "racket" or ft == "scheme" or ft == "lisp" then
    cmd = '(enter! "' .. vim.fs.basename(file) .. '")'
  elseif ft == "haskell" then
    cmd = ":r"
  elseif ft == "scala" then
    cmd = ':load "' .. file .. '"'
  elseif ft == "elixir" then
    cmd = 'c("' .. file .. '")'
  else
    vim.notify("No slime runner for filetype: " .. ft, vim.log.levels.WARN)
    return
  end

  slime_send(cmd)
end

local function smart_send(text)
  if vim.bo.filetype == "haskell" then
    local trimmed = text:gsub("%s+$", "")
    if trimmed:find("\n") then
      text = ":{\n" .. text
      if not text:match("\n$") then
        text = text .. "\n"
      end
      text = text .. ":}\n"
    end
  end
  if not text:match("\n$") then
    text = text .. "\n"
  end
  vim.fn["slime#send"]({ text })
end

local function slime_send_region()
  local save_reg = vim.fn.getreg("z")
  local save_type = vim.fn.getregtype("z")
  vim.cmd('noautocmd normal! gv"zy')
  local text = vim.fn.getreg("z")
  vim.fn.setreg("z", save_reg, save_type)
  smart_send(text)
end

local function slime_send_paragraph()
  local save_reg = vim.fn.getreg("z")
  local save_type = vim.fn.getregtype("z")
  vim.cmd('noautocmd normal! vip"zy')
  local text = vim.fn.getreg("z")
  vim.fn.setreg("z", save_reg, save_type)
  smart_send(text)
end

return {
  {
    "aserowy/tmux.nvim",
    keys = {
      {
        "<M-H>",
        function()
          require("tmux").resize_left()
        end,
        mode = { "n", "t" },
        desc = "Resize left",
      },
      {
        "<M-J>",
        function()
          require("tmux").resize_bottom()
        end,
        mode = { "n", "t" },
        desc = "Resize down",
      },
      {
        "<M-K>",
        function()
          require("tmux").resize_top()
        end,
        mode = { "n", "t" },
        desc = "Resize top",
      },
      {
        "<M-L>",
        function()
          require("tmux").resize_right()
        end,
        mode = { "n", "t" },
        desc = "Resize down",
      },
      {
        "<M-h>",
        function()
          require("tmux").move_left()
        end,
        mode = { "n", "t" },
        desc = "Move left",
      },
      {
        "<M-j>",
        function()
          require("tmux").move_bottom()
        end,
        mode = { "n", "t" },
        desc = "Move down",
      },
      {
        "<M-k>",
        function()
          require("tmux").move_top()
        end,
        mode = { "n", "t" },
        desc = "Move up",
      },
      {
        "<M-l>",
        function()
          require("tmux").move_right()
        end,
        mode = { "n", "t" },
        desc = "Move rigth",
      },
    },
    opts = {
      navigation = {
        persist_zoom = true,
        enable_default_keybindings = false,
      },
      resize = {
        enable_default_keybindings = false,
      },
    },
  },
  {
    "jpalardy/vim-slime",
    keys = {
      { mode = "x", "<C-c><C-c>", slime_send_region, desc = "Send region to tmux" },
      { mode = "n", "<C-c><C-c>", slime_send_paragraph, desc = "Send block to tmux" },
      { mode = "n", "<C-c><C-s>", slime_send_file, desc = "Source file to tmux" },
      { mode = "n", "<C-c><C-l>", "<cmd>SlimeSendCurrentLine<cr>", desc = "Send line to tmux" },
    },
    init = function()
      vim.g.slime_no_mappings = 1
      vim.g.slime_dont_ask_default = 1
      vim.g.slime_bracketed_paste = 1
    end,
    config = function(_, _)
      vim.g.slime_target = "tmux"
      vim.g.slime_default_config = {
        socket_name = "default",
        target_pane = "{last}",
      }
    end,
  },
}
