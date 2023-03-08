-- Neovim tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup({
  actions = {
    open_file = {
      quit_on_open = true
    }
  }
})
vim.api.nvim_create_autocmd({ "QuitPre" }, {
  callback = function() vim.cmd("NvimTreeClose") end,
})
vim.keymap.set('n', '<F2>', '<cmd>NvimTreeToggle<cr>')
vim.keymap.set('n', '<leader><F2>', '<cmd>NvimTreeFindFile<cr>')

-- Theme
vim.opt.background = 'dark'
vim.opt.showmode = false
vim.g.zenbones_compat = 1
vim.cmd[[colorscheme zenbones]]
require('nvim-web-devicons').setup()
require('lualine').setup({
  options = {
    disabled_filetypes = { 'NvimTree', 'packer', 'Mundo', 'ctrlsf' }
  },
  sections = {
    lualine_c = { require('auto-session-library').current_session_name }
  }
})

-- Surround
require('nvim-surround').setup()

-- Terminal
require("toggleterm").setup({
  open_mapping = [[<c-\>]],
})

-- Gitsigns
require('gitsigns').setup({
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
    end, { expr = true })

    map('n', '[c', function()
      if vim.wo.diff then return '[c' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
    end, { expr = true })

    -- Actions
    map({ 'n', 'v' }, '<leader>hs', ':Gitsigns stage_hunk<CR>')
    map({ 'n', 'v' }, '<leader>hr', ':Gitsigns reset_hunk<CR>')
    map('n', '<leader>hS', gs.stage_buffer)
    map('n', '<leader>hu', gs.undo_stage_hunk)
    map('n', '<leader>hR', gs.reset_buffer)
    map('n', '<leader>hp', gs.preview_hunk)
    map('n', '<leader>hb', function() gs.blame_line { full = true } end)
    map('n', '<leader>tb', gs.toggle_current_line_blame)
    map('n', '<leader>hd', gs.diffthis)
    map('n', '<leader>hD', function() gs.diffthis('~') end)
    map('n', '<leader>td', gs.toggle_deleted)

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
    border = "double",
  },
  on_open = function(term)
    vim.cmd("startinsert!")
    vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", { noremap = true, silent = true })
  end,
  on_close = function(_)
    vim.cmd("startinsert!")
  end,
})

local function lazygit_toggle()
  lazygit:toggle()
end
vim.keymap.set("n", "<leader>g", lazygit_toggle, { noremap = true, silent = true })

-- Tree sitter
local parsers_dir = vim.g.vim_home .. '/parsers'
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
  auto_session_root_dir = vim.g.vim_home .. '/sessions/'
})

-- Undo
vim.opt.undodir = vim.g.vim_home .. '/undofiles'
vim.opt.undofile = true
vim.keymap.set('n', '<leader>u', '<cmd>MundoToggle<cr>')

-- FzF
vim.keymap.set('n', '<c-P>', "<cmd>lua require('fzf-lua').files()<CR>", { noremap = true, silent = true })

-- CtrlSF search
vim.g.ctrlsf_auto_focus = {
  at = "start"
}
vim.keymap.set('n', '<leader>/', '<Plug>CtrlSFPrompt', { noremap = true })
vim.keymap.set('v', '<leader>/', '<Plug>CtrlSFVwordPath', { noremap = true })
vim.keymap.set('n', '<F3>', '<cmd>CtrlSFToggle<cr>', { noremap = true, silent = true })

-- Spectre search
require('spectre').setup({
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

-- Autocomplete cmp
require('./configs/autocomplete')

-- Perfomance
_G.__luacache_config = {
  chunks = {
    enable = true,
    path = vim.g.vim_home .. '/luacache_chunks',
  },
  modpaths = {
    enable = true,
    path = vim.g.vim_home .. '/luacache_modpaths',
  }
}
require('impatient')
