local g = vim.g
local o = vim.opt

-- Map <leader> to space
g.mapleader = ' '
g.maplocalleader = ' '

-- Set color
o.termguicolors = true

-- Encoding
o.encoding = 'utf-8'
o.fileencoding = 'utf-8'

-- Fix backspace indent
o.backspace = 'indent,eol,start'

-- Enable hidden buffers
o.hidden = true

-- No swap, no backup
o.backup = false
o.swapfile = false

-- Number of screen lines to keep above and below the cursor
o.scrolloff = 8
o.number = true
o.relativenumber = true
o.cursorline = true

-- Title
o.title = true

-- Interval update time
o.updatetime = 100

-- Search
o.grepprg = 'ag --nogroup --nocolor'
o.incsearch = true
o.hlsearch = true
o.ignorecase = true
vim.keymap.set('n', '<esc>', '<cmd>noh<cr><esc>', { silent = true, noremap = true })

-- Don't wrap line
o.wrap = true
o.linebreak = true
o.breakindent = true

-- Tab options
o.tabstop = 2
o.softtabstop = 2
o.shiftwidth = 2
o.expandtab = true

-- Hightlight current line
o.cursorline = true

-- Update external change to the buffer
o.autoread = true

-- Clipboard support
vim.keymap.set({'n', 'v'}, '<leader>y', '"+y')
vim.keymap.set({'n', 'v'}, '<leader>p', '"+p')

-- Jump to the last place in the file before exiting
vim.api.nvim_create_autocmd('BufReadPost', {
  callback = function(data)
    local last_pos = vim.api.nvim_buf_get_mark(data.buf, '"')
    if last_pos[1] > 0 and last_pos[1] <= vim.api.nvim_buf_line_count(data.buf) then
      vim.api.nvim_win_set_cursor(0, last_pos)
    end
  end,
})
