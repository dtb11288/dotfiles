local packpath = VIM_HOME
local bundle_home = packpath .. '/pack'
local packer_path = bundle_home .. '/packer/start/packer.nvim'

vim.opt.runtimepath:append(packpath)
vim.opt.packpath = packpath

-- Automatically run :PackerCompile whenever plugins.lua is updated with an autocommand:
vim.api.nvim_create_autocmd('BufWritePost', {
  pattern = 'plugins.lua',
  command = 'source <afile> | PackerCompile',
})

if not vim.loop.fs_stat(packer_path) then
  print('Packer not found, clone repository...')
  vim.fn.system({ 'mkdir', '-p', bundle_home })
  PACKER_BOOTSTRAP = vim.fn.system({
    'git',
    'clone',
    '--depth',
    '1',
    'https://github.com/wbthomason/packer.nvim', packer_path
  })
  vim.cmd [[ packadd packer.nvim ]]
end

return require('packer').startup({
  function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Perfomance
    use 'lewis6991/impatient.nvim'

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
    use 'hrsh7th/cmp-nvim-lsp-signature-help'
    use 'saadparwaiz1/cmp_luasnip'
    use 'j-hui/fidget.nvim'

    -- Rust
    use 'simrat39/rust-tools.nvim'

    -- Javascript
    use 'jose-elias-alvarez/null-ls.nvim'
    use 'MunifTanjim/eslint.nvim'

    -- Control
    use 'ibhagwan/fzf-lua'
    use 'dyng/ctrlsf.vim'
    use 'windwp/nvim-spectre'

    -- Editor
    use 'numToStr/Comment.nvim'
    use 'mg979/vim-visual-multi'
    use 'kylechui/nvim-surround'
    use 'junegunn/vim-easy-align'
    use 'lewis6991/spaceless.nvim'
    use 'Chiel92/vim-autoformat'
    use 'moll/vim-bbye'
    use 'jiangmiao/auto-pairs'
    use 'L3MON4D3/LuaSnip'
    use 'folke/which-key.nvim'

    -- History & Session
    use 'rmagatti/auto-session'
    use 'simnalamburt/vim-mundo'

    -- Post-install/update hook with neovim command
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    -- Theme
    use 'nvim-lualine/lualine.nvim'
    use 'nvim-tree/nvim-web-devicons'
    use 'mcchrish/zenbones.nvim'

    if PACKER_BOOTSTRAP then
      require("packer").sync()
    end
  end,
  config = {
    package_root = bundle_home
  }
})
