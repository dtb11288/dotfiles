-- Neovim tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup()
vim.keymap.set('n', '<leader>e', '<cmd>NvimTreeToggle<cr>')
vim.keymap.set('n', '<leader>E', '<cmd>NvimTreeFindFile<cr>')

-- Theme
vim.o.background = 'dark'
require('noirbuddy').setup {
  preset = 'slate',
}
require('nvim-web-devicons').setup()
require('lualine').setup({
  options = {
    disabled_filetypes = { 'NvimTree', 'packer', 'Mundo' }
  }
})

-- Surround
require('nvim-surround').setup()

-- Terminal
require("toggleterm").setup({
  open_mapping = [[<c-\>]],
})

-- Lazygit
local Terminal  = require('toggleterm.terminal').Terminal
local lazygit = Terminal:new({
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
vim.o.undodir = vim.g.vim_home .. '/undofiles'
vim.o.undofile = true
vim.keymap.set('n', '<leader>u', '<cmd>MundoToggle<cr>')

-- FzF
vim.keymap.set('n', '<c-P>', "<cmd>lua require('fzf-lua').files()<CR>", { noremap = true, silent = true })

-- Autocomplete cmp
require('./configs/autocomplete')
