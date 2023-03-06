local g = vim.g
local opt = vim.opt

-- Automatically run :PackerCompile whenever plugins.lua is updated with an autocommand:
vim.api.nvim_create_autocmd('BufWritePost', {
  group = vim.api.nvim_create_augroup('PACKER', { clear = true }),
  pattern = 'plugins.lua',
  command = 'source <afile> | PackerCompile',
})

local packpath = g.vim_home
opt.runtimepath:append(packpath)
opt.packpath = packpath

local bundle_home = packpath .. '/pack'
local packer_path = bundle_home .. '/packer/start/packer.nvim'
if (vim.fn.empty(vim.fn.glob(packer_path))) > 0 then
  print('Packer not found, clone repository...')
  vim.fn.system({'mkdir', '-p', bundle_home})
  vim.fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', packer_path})
  vim.cmd [[ packadd packer.nvim ]]
end

return require('packer').startup({
  function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- File explorer
    use 'nvim-tree/nvim-tree.lua'

    -- Terminal
    use 'akinsho/toggleterm.nvim'

    -- Git support
    use 'nvim-lua/plenary.nvim'
    use 'lewis6991/gitsigns.nvim'
    use 'sindrets/diffview.nvim'

    -- Autocomplete
    use 'neovim/nvim-lspconfig'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/cmp-cmdline'
    use 'hrsh7th/nvim-cmp'
    use 'saadparwaiz1/cmp_luasnip'

    -- Control
    use 'nvim-lualine/lualine.nvim'
    use 'simnalamburt/vim-mundo'
    use 'haya14busa/incsearch.vim'
    use 'ibhagwan/fzf-lua'
    use 'dyng/ctrlsf.vim'

    -- Editor
    use 'tomtom/tcomment_vim'
    use 'terryma/vim-multiple-cursors'
    use 'kylechui/nvim-surround'
    use 'junegunn/vim-easy-align'
    use 'ntpeters/vim-better-whitespace'
    use 'djoshea/vim-autoread'
    use 'Chiel92/vim-autoformat'
    use 'moll/vim-bbye'
    use 'jiangmiao/auto-pairs'
    use 'neomake/neomake'
    use 'L3MON4D3/LuaSnip'

    -- Session
    use 'rmagatti/auto-session'

    -- Post-install/update hook with neovim command
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    -- Theme
    use 'tjdevries/colorbuddy.nvim'
    use 'jesseleite/nvim-noirbuddy'
    use 'nvim-tree/nvim-web-devicons'
  end,
  config = {
    package_root = bundle_home
  }
})
