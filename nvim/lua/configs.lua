-- Neovim tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup({
  actions = {
    open_file = {
      quit_on_open = true
    }
  },
  git = {
    ignore = false,
  },
  renderer = {
    icons = {
      show = {
        git = true,
        file = false,
        folder = false,
      },
      glyphs = {
        folder = {
          arrow_closed = '>',
          arrow_open = '>>>',
        },
        git = {
          unstaged = '+',
          staged = '*',
          unmerged = '^',
          renamed = '&',
          untracked = '?',
          deleted = '-',
          ignored = '!',
        },
      }
    }
  }
})
vim.api.nvim_create_autocmd({ "QuitPre" }, {
  callback = function()
    vim.cmd('DBUIClose')
    vim.cmd('NvimTreeClose')
  end,
})
vim.keymap.set('n', '<F2>', function () require('nvim-tree.api').tree.toggle({ focus = true }) end)
vim.keymap.set('n', '<leader><F2>', function () require('nvim-tree.api').tree.toggle({ find_file = true, focus = true }) end)

-- Autocomplete cmp
require('./configs/autocomplete')

-- Theme
require('./configs/theme')

-- Key helper
require('which-key').setup()

-- Surround
require('nvim-surround').setup()

-- Terminal
require("toggleterm").setup({
  open_mapping = [[<c-\>]],
})

-- Dadbod
vim.g.db_ui_execute_on_save = 0
vim.g.db_ui_icons = {
  expanded = '>>>',
  collapsed = '>',
  saved_query = '*',
  new_query = '+',
  tables = '~',
  buffers = '**',
  connection_ok = '(ok)',
  connection_error = '(err)',
}

-- Gitsigns
require('gitsigns').setup({
  preview_config = {
    border = BORDER,
  },
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
    untracked    = { text = '.' },
  },
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map('n', ']c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(function() gs.next_hunk() end)
      return '<Ignore>'
    end, { expr = true, desc = 'Jump To Next Hunk' })

    map('n', '[c', function()
      if vim.wo.diff then return '[c' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
    end, { expr = true, desc = 'Jump To Previous Hunk' })

    -- Actions
    map({ 'n', 'v' }, '<leader>hs', ':Gitsigns stage_hunk<CR>', { desc = 'Stage hunk' })
    map({ 'n', 'v' }, '<leader>hr', ':Gitsigns reset_hunk<CR>', { desc = 'Reset hunk' })
    map('n', '<leader>hS', gs.stage_buffer, { desc = 'Stage current buffer' })
    map('n', '<leader>hu', gs.undo_stage_hunk, { desc = 'Undo stage hunk' })
    map('n', '<leader>hR', gs.reset_buffer, { desc = 'Reset buffer' })
    map('n', '<leader>hp', gs.preview_hunk, { desc = 'Preview hunk' })
    map('n', '<leader>hb', function() gs.blame_line { full = true } end, { desc = 'Lines blame' })
    map('n', '<leader>tb', gs.toggle_current_line_blame, { desc = 'Toggle current line blame' })
    map('n', '<leader>hd', gs.diffthis, { desc = 'Current diff preview' })
    map('n', '<leader>hD', function() gs.diffthis('~') end, { desc = 'Previous diff preview' })
    map('n', '<leader>td', gs.toggle_deleted, { desc = 'Toggle deleted hunk' })

    -- Text object
    map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
  end
})

-- Lazygit
local Terminal = require('toggleterm.terminal').Terminal
local lazygit  = Terminal:new({
  cmd = "lazygit",
  dir = "git_dir",
  direction = "float",
  float_opts = {
    border = BORDER,
  },
  on_open = function(term)
    vim.cmd("startinsert!")
    vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", { noremap = true, silent = true })
  end,
  on_close = function(_)
    vim.cmd("startinsert!")
  end,
})
vim.keymap.set('n', '<leader>g', function () lazygit:toggle() end, { noremap = true, silent = true, desc = 'Open Lazygit' })

-- Tree sitter
local parsers_dir = VIM_HOME .. '/parsers'
require('nvim-treesitter.configs').setup {
  auto_install = true,
  parser_install_dir = parsers_dir,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}
vim.opt.runtimepath:append(parsers_dir)

-- Sessions
require('auto-session').setup({
  auto_session_root_dir = VIM_HOME .. '/sessions/'
})

-- Undo
vim.opt.undodir = VIM_HOME .. '/undofiles'
vim.opt.undofile = true
vim.keymap.set('n', '<leader>u', '<cmd>MundoToggle<cr>')

-- Close buffers
vim.keymap.set('n', '<leader>cb', ':Bdelete<cr>', { noremap = true, silent = true, desc = 'Close current buffer' })
vim.keymap.set('n', '<leader>cab', ':bufdo Bdelete<cr>', { noremap = true, silent = true, desc = 'Close all buffers' })

-- Tab
vim.keymap.set('n', '<leader>ct', ':tabclose<cr>', { noremap = true, silent = true, desc = 'Close current tab' })
vim.keymap.set('n', '<leader>tn', ':tabnew<cr>', { noremap = true, silent = true, desc = 'Open new tab' })

-- Whitespace remover
require('trim').setup()

-- FzF
local fzf = require('fzf-lua')
fzf.setup({
  winopts = {
    border = BORDER
  }
})
vim.keymap.set('n', '<leader>f', fzf.files, { noremap = true, silent = true, desc = 'Search Files' })
vim.keymap.set('n', '<leader>b', fzf.buffers, { noremap = true, silent = true, desc = 'Search Buffers' })

-- Comment
require('Comment').setup()

-- CtrlSF search
vim.g.ctrlsf_auto_focus = {
  at = "start"
}
vim.keymap.set('n', '<leader>/', '<Plug>CtrlSFPrompt', { noremap = true })
vim.keymap.set('v', '<leader>/', '<Plug>CtrlSFVwordPath', { noremap = true })
vim.keymap.set('n', '<F3>', '<cmd>CtrlSFToggle<cr>', { noremap = true, silent = true })

-- Spectre search
require('spectre').setup({
  color_devicons = false,
  default = {
    find = {
      cmd = "ag",
      options = {"ignore-case"}
    },
    replace={
      cmd = "sed"
    }
  },
})
vim.cmd[[nnoremap <leader>S <cmd>lua require('spectre').open()<CR>]]
-- search current word
vim.cmd[[nnoremap <leader>sw <cmd>lua require('spectre').open_visual({select_word=true})<CR>]]
vim.cmd[[vnoremap <leader>s <esc>:lua require('spectre').open_visual()<CR>]]
-- search in current file
vim.cmd[[nnoremap <leader>sp viw:lua require('spectre').open_file_search()<cr>]]

-- Perfomance
_G.__luacache_config = {
  chunks = {
    enable = true,
    path = VIM_HOME .. '/luacache_chunks',
  },
  modpaths = {
    enable = true,
    path = VIM_HOME .. '/luacache_modpaths',
  }
}
require('impatient')
