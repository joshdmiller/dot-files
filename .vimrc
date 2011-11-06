" syntax highlighting
:syntax on

" toggle line-numbering with F2
:nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" auto-indent
:set smartindent

" expand tabs to spaces
:set expandtab

" tabs are 4 spaces
:set shiftwidth=4
:set softtabstop=4

" auto-detect file types
:filetype plugin on
:filetype indent on

:set nocompatible
:set guifont=Liberation\ Mono\ 10
:let g:bmenu_max_pathlen=0

" Pathogen package management plugin
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" special formatting for python
autocmd FileType python set complete+=k~/.vim/syntax/python.vim isk+=.,(
" Execute file being edited with <Shift> + e:
map <buffer> <S-e> :w<CR>:!/usr/bin/env python % <CR>

" mark as error going over 80 chars
:match ErrorMsg /\%>73v.\+/

" markas error any tab characters
:match errorMsg /[\t]/

" mark todo a certain color
" :highlight todoKeyword term=bold ctermfg=green guifg=green
" :match todoKeyword /\s*TODO/

" set the status line
" :set statusline=%F%m%r%h%w\ [TYPE=%Y]\ [POS=%04l,%04v]\ [%p%%]\ [LEN=%L]

" mark the line we're on
:set cursorline
:set number

" map some convenience keys
:imap <C-s> <ESC>:w<CR>a
:nmap <C-s> :w<CR>
:imap <C-p> <ESC>p<CR>a

" set up some nice things for writing
function! StartWriting()
    set spell
    set spellsuggest=10
    set textwidth=80
    set wm=2
    set thesaurus+=~/.vim/mthesaur.txt
    filetype indent off
endfunction

function! StartCoding()
    set nospell
    set textwidth=0
    filetype indent on
    set smartindent
endfunction

:map <F7> :call StartWriting()<CR>
:map <F9> :call StartCoding()<CR>

au BufNewFile,BufRead *.scala setfiletype scala

" vala
autocmd BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
autocmd BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala            setfiletype vala
au BufRead,BufNewFile *.vapi            setfiletype vala
" Disable valadoc syntax highlight
"let vala_ignore_valadoc = 1

" Enable comment strings
let vala_comment_strings = 1

" Highlight space errors
let vala_space_errors = 1
" Disable trailing space errors
"let vala_no_trail_space_error = 1
" Disable space-tab-space errors
let vala_no_tab_space_error = 1

" Minimum lines used for comment syncing (default 50)
"let vala_minlines = 120
"
" ignore some additional file patterns for the NERDTree filter
:let NERDTreeIgnore=['\.o$','\.swp$','\~$']

" syntax highlighting for yaml
au BufNewFile,BufRead *.yaml,*.yml setfiletype yaml

