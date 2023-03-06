local g = vim.g
local o = vim.o
local a = vim.api

-- Map <leader> to space
g.mapleader = ' '
g.maplocalleader = ' '

o.termguicolors = true

-- Encoding
o.encoding = 'utf-8'
o.fileencoding = 'utf-8'

-- Fix backspace indent
o.backspace = 'indent,eol,start'

-- Enable hidden buffers
o.hidden = true

-- No swap, no backup
o.nobackup = true
o.noswapfile = true

-- Number of screen lines to keep above and below the cursor
o.scrolloff = 8

-- Title
o.title = true

-- Interval update time
o.updatetime=100

-- Search
o.grepprg='ag --nogroup --nocolor'
o.incsearch = true
o.hlsearch = true
o.ignorecase = true
-- nnoremap <silent><esc> :noh<cr><esc>

-- Don't wrap line
o.wrap = true
o.linebreak = true
o.breakindent = true

-- Tab options
o.tabstop = 2
o.softtabstop = 2
o.shiftwidth = 2
o.expandtab = true

-- Jump to the last place in the file before exiting
a.nvim_create_autocmd('BufReadPost', {
    group = num_au,
    callback = function(data)
        local last_pos = A.nvim_buf_get_mark(data.buf, '"')
        if last_pos[1] > 0 and last_pos[1] <= A.nvim_buf_line_count(data.buf) then
            A.nvim_win_set_cursor(0, last_pos)
        end
    end,
})
