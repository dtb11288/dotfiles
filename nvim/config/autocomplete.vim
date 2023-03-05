" deoplete
" let g:deoplete#enable_at_startup = 1
" call deoplete#custom#var('deoplete#auto_complete_start_length', v:true)
" call deoplete#custom#var('file', 'enable_buffer_path', v:true)
" call deoplete#custom#var('deoplete#enable_smart_case', v:true)
set completeopt-=preview

" lsp
" let g:LanguageClient_serverCommands = {
"       \ 'dart': ['dart_language_server'],
"       \ 'rust': ['rust-analyzer'],
"       \ 'javascript': ['javascript-typescript-stdio'],
"       \ 'typescript': ['javascript-typescript-stdio'],
"       \ 'go': ['go-langserver'],
"       \ 'python': ['pyls'],
"       \ }
" nnoremap <silent><leader>ll :call LanguageClient_contextMenu()<cr>
" nnoremap <silent><leader>lh :call LanguageClient#textDocument_hover()<cr>
" nnoremap <silent>gd :call LanguageClient#textDocument_definition()<cr>
" nnoremap <silent><leader>lr :call LanguageClient#textDocument_rename()<cr>

" Use <c-space> to trigger completion
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Applying code actions to the selected code block
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying code actions at the cursor position
nmap <leader>ac  <Plug>(coc-codeaction-cursor)
" Remap keys for apply code actions affect whole buffer
nmap <leader>as  <Plug>(coc-codeaction-source)
" Apply the most preferred quickfix action to fix diagnostic on the current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Remap keys for applying refactor code actions
nmap <silent> <leader>re <Plug>(coc-codeaction-refactor)
xmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
nmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)

" Run the Code Lens action on the current line
nmap <leader>cl  <Plug>(coc-codelens-action)

" Auto import when enter
inoremap <expr> <cr> coc#pum#visible() ? coc#pum#confirm() : "\<CR>"

let g:coc_global_extensions = ['coc-tabnine', 'coc-sql', 'coc-rust-analyzer', 'coc-highlight', 'coc-json']

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')
