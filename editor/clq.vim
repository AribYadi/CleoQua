" Quick Start
" Put this file into ~/.vim/syntax/ for vim and
" /usr/local/share/nvim/runtime/syntax for neovim
" Then put the following line into your .vimrc / init.vim without the double
" quote
" autocmd BufRead,BufNewFile *.clq set ft=clq

if exists("b:current_syntax")
	finish
endif

set iskeyword=a-z,A-Z,48-57
syn keyword clqKeyword if else while do end macro
syn match clqKeyword /\v(\s|^)@<=syscall[0-6](\s|$)@=/

syn keyword clqTodo TODO NOTE WARN
syn region clqComment start="#" end="$" contains=clqTodo

syn region clqString start=/\v(\s|^)@<="/ skip=/\v\\./ end=/\v"(\s|$)@=/
syn match clqCharacter /\v(\s|^)@<='.'(\s|$)@=/

syn match clqInt /\v(\s|^)@<=-*[0-9]+(\s|$)@=/

hi def link clqKeyword   Keyword
hi def link clqTodo      Todo
hi def link clqComment   Comment
hi def link clqString    String
hi def link clqCharacter Character
hi def link clqInt       Number

let b:current_syntax = "clq"

