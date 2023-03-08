vim.opt.background = 'dark'
vim.opt.showmode = false
vim.g.zenbones_compat = 1
vim.cmd [[colorscheme zenbones]]
require('nvim-web-devicons').setup()
require('lualine').setup({
  options = {
    disabled_filetypes = { 'NvimTree', 'packer', 'Mundo', 'ctrlsf' }
  },
  sections = {
    lualine_c = { require('auto-session-library').current_session_name }
  }
})
vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'black', fg = 'black' })
vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'black' })
