" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 1
let g:deoplete#file#enable_buffer_path = 1
let g:deoplete#enable_smart_case = 1
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
nnoremap <silent><leader>lh :call LanguageClient#textDocument_hover()<cr>
nnoremap <silent><leader>ld :call LanguageClient#textDocument_definition()<cr>
nnoremap <silent><leader>lr :call LanguageClient#textDocument_rename()<cr>
