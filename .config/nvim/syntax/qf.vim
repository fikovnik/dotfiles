" modified https://gitlab.com/yorickpeterse/nvim-pqf
if exists('b:current_syntax')
  finish
end

syntax clear

let b:current_syntax = 'qf'

lua require('quickfix').syntax()
