let s:tslint_path = system('PATH=$(npm bin):$PATH && which tslint')
let b:neomake_typescript_tslint_exe = substitute(s:tslint_path, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
let g:neomake_typescript_enabled_makers = ['tslint']
