vim.opt.background = 'dark'
vim.opt.showmode = false
vim.g.zenbones_compat = 1
vim.cmd [[colorscheme zenbones]]
require('lualine').setup({
  options = {
    disabled_filetypes = { 'NvimTree', 'packer', 'Mundo', 'ctrlsf', 'toggleterm' },
    icons_enabled = false,
    theme = 'nord',
    component_separators = '|',
    section_separators = '',
  },
  sections = {
    lualine_c = { require('auto-session-library').current_session_name }
  }
})
require('colorizer').setup()
