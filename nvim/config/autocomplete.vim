" deoplete
let g:deoplete#enable_at_startup = 1
call deoplete#custom#var('deoplete#auto_complete_start_length', v:true)
call deoplete#custom#var('file', 'enable_buffer_path', v:true)
call deoplete#custom#var('deoplete#enable_smart_case', v:true)
set completeopt-=preview

" lsp
let g:LanguageClient_serverCommands = {
      \ 'dart': ['dart_language_server'],
      \ 'rust': ['rust-analyzer'],
      \ 'javascript': ['javascript-typescript-stdio'],
      \ 'typescript': ['javascript-typescript-stdio'],
      \ 'go': ['go-langserver'],
      \ 'python': ['pyls'],
      \ }
nnoremap <silent><leader>ll :call LanguageClient_contextMenu()<cr>
nnoremap <silent><leader>lh :call LanguageClient#textDocument_hover()<cr>
nnoremap <silent><leader>ld :call LanguageClient#textDocument_definition()<cr>
nnoremap <silent><leader>lr :call LanguageClient#textDocument_rename()<cr>
