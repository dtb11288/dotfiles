-- Define global variables
local g = vim.g
g.vim_home = vim.fn.expand('~/.config/nvim')

require('settings')
require('plugins')
require('configs')
