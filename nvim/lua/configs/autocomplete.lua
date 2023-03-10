-- Load snippets
require("luasnip.loaders.from_snipmate").lazy_load()

-- LSP diagnostics setup
local sign = function(opts)
  vim.fn.sign_define(opts.name, {
    texthl = opts.name,
    text = opts.text,
    numhl = ''
  })
end

sign({ name = 'DiagnosticSignError', text = 'E' })
sign({ name = 'DiagnosticSignWarn', text = 'W' })
sign({ name = 'DiagnosticSignHint', text = 'H' })
sign({ name = 'DiagnosticSignInfo', text = 'I' })

vim.diagnostic.config({
  float = {
    border = BORDER,
  },
})

-- Cmp config
local cmp = require('cmp')
local select_opts = { behavior = cmp.SelectBehavior.Insert }
local luasnip = require('luasnip')
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }
vim.opt.shortmess = vim.opt.shortmess + { c = true }
cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  window = {
    completion = {
      border = BORDER,
    },
    documentation = {
      border = BORDER,
    },
  },
  formatting = {
    fields = { 'menu', 'abbr', 'kind' },
    format = function(entry, item)
      local menu_icon = {
        nvim_lsp = '[L]',
        luasnip = '[S]',
        buffer = '[B]',
        path = '[P]',
        cmdline = '[C]'
      }
      item.menu = menu_icon[entry.source.name]
      return item
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
    ['<C-n>'] = cmp.mapping.select_next_item(select_opts),
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = false }),
    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
    ['<C-f>'] = cmp.mapping(function(fallback)
      if luasnip.jumpable(1) then
        luasnip.jump(1)
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-b>'] = cmp.mapping(function(fallback)
      if luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources({
    { name = 'luasnip' },
    { name = 'path' },
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'buffer' },
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { noremap = true, silent = true, desc = 'Open Diagnostic Window' })
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { noremap = true, silent = true, desc = 'Jump To Previous Diagnostic' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { noremap = true, silent = true, desc = 'Jump To Next Diagnostic' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { noremap = true, silent = true, desc = 'Open Diagnostic List' })

local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc, noremap = true, silent = true })
  end

  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  nmap('gD', vim.lsp.buf.declaration, 'Goto Declaration')
  nmap('gd', vim.lsp.buf.definition, 'Goto Definition')
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('gi', vim.lsp.buf.implementation, 'Goto Implementation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, 'Workspace Add Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, 'Workspace Remove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, 'Workspace List Folders')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type Definition')
  nmap('<leader>rn', vim.lsp.buf.rename, 'Rename')
  nmap('<leader>ca', vim.lsp.buf.code_action, 'Code Action')
  nmap('gr', require('fzf-lua').lsp_references, 'Goto References')
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- Set up lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()
local lspconfig = require('lspconfig')
local lsp_defaults = lspconfig.util.default_config
lsp_defaults.on_attach = on_attach
lsp_defaults.capabilities = vim.tbl_deep_extend(
  'force',
  lsp_defaults.capabilities,
  capabilities
)

-- Small UI for progress
require('fidget').setup()

-- Rust
local rust_tools = require('rust-tools')
rust_tools.setup()
rust_tools.inlay_hints.enable()

lspconfig.lua_ls.setup {}
lspconfig.tsserver.setup {}
lspconfig.lua_ls.setup {
  settings = {
    Lua = {
      diagnostics = {
        globals = { 'vim' },
      }
    }
  }
}

-- Javascript
require("null-ls").setup()
require("eslint").setup({
  bin = 'eslint', -- or `eslint_d`
  code_actions = {
    enable = true,
    apply_on_save = {
      enable = true,
      types = { "directive", "problem", "suggestion", "layout" },
    },
    disable_rule_comment = {
      enable = true,
      location = "separate_line", -- or `same_line`
    },
  },
  diagnostics = {
    enable = true,
    report_unused_disable_directives = false,
    run_on = "type", -- or `save`
  },
})
