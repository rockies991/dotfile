filetype off     
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'rudrab/vimf90' 
Plugin 'ervandew/supertab'
"Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'Buffergator'
Plugin 'scrooloose/nerdcommenter'
Plugin 'honza/vim-snippets'
Plugin 'scrooloose/nerdtree'
Plugin 'Shougo/deoplete.nvim'
Plugin 'roxma/nvim-yarp'
Plugin 'roxma/vim-hug-neovim-rpc'
Plugin 'Shougo/neosnippet.vim'
Plugin 'Shougo/neosnippet-snippets'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating'
Plugin 'mattn/calendar-vim'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'ctrlp.vim'
Plugin 'ncm2/ncm2'
Plugin 'ncm2/ncm2-bufword'
Plugin 'ncm2/ncm2-path'
Plugin 'ncm2/ncm2-tern'
Plugin 'ncm2/ncm2-ultisnips'
Plugin 'Yggdroot/indentLine'
Plugin 'miyakogi/conoline.vim'
Plugin 'posva/vim-vue'
Plugin 'zxqfl/tabnine-vim'
call vundle#end()            " required


filetype plugin indent on    " required
set autoindent
set smartindent
set cindent 
set ic
set incsearch
set nocp incsearch
set cinwords=if,else,while,do,for,switch,case
set formatoptions=tcqr
set showmatch
set smartcase
set makeprg=make
set ruler
set wrap
set nohls

:let fortran_free_source=1

:runtime plugin/*.vim
:runtime ftplugin/*.vim

:set wildmenu
:set cpo-=<
:set wcm=<C-Z>
:syntax on
set tags=./tags,tags,$HOME

:autocmd BufReadPost *.doc %!antiword "%"

:set mouse=r
:set backspace=indent,eol,start
:set smarttab
:set copyindent
:set nobackup
:set title
:set noswapfile
:nnoremap ; :

filetype on
filetype indent on
filetype plugin on

:set csto=1

let g:tex_flavor='latex'
set grepprg=grep\ -nH\ $*

let g:Tex_Folding=0 "I don't like folding.
set iskeyword+=:

:imap ;; <Esc>
":set virtualedit=all

if has('cscope')
    set cscopetag cscopeverbose

    if has('quickfix')
        set cscopequickfix=s-,c-,d-,i-,t-,e-
    endif

    cnoreabbrev csa cs add
    cnoreabbrev csf cs find
    cnoreabbrev csk cs kill
    cnoreabbrev csr cs reset
    cnoreabbrev css cs show
    cnoreabbrev csh cs help
endif

let g:ctags_regenerate = 0 
let g:ctags_statusline=1


let Tlist_WinWidth = 50
map <F4> :TlistToggle<cr>
map <F8> :!/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

highlight Type ctermfg=white
highlight Keyword ctermfg=red
highlight vimCommand ctermfg=red
highlight Operator ctermfg=white
highlight Repeat ctermfg=red
highlight Conditional ctermfg=white
highlight Label ctermfg=white
highlight Comment ctermfg=green
highlight String ctermfg=green
highlight Number ctermfg=green

au BufNewFile,BufRead *.cu set ft=cu
au BufNewFile,BufRead *.cuh set ft=cu
au BufNewFile,BufRead *.cuf set ft=fortran
:syn on

":highlight LineNr ctermfg=Black ctermbg=Gray
":set cursorline

let fortran_do_enddo=1

:function! ReverseBackground()
: let Mysyn=&syntax
: if &bg=="light"
: se bg=dark
: else
: se bg=light
: endif
: syn on
: exe "set syntax=" . Mysyn
: echo "now syntax is "&syntax
:endfunction
:command! Invbg call ReverseBackground()

:map <F11> :let &background = ( &background == "dark"? "light" : "dark" )<CR>
:inoremap \fp <C-R>=getcwd()<C-R>

:set mousemodel=popup

:filetype on
autocmd FileType tex call Fix_latex()
function! Fix_latex()
    syntax spell toplevel
    set spell
endfunction

map ,hi :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>
:set background=dark

:hi CursorLine   cterm=NONE ctermbg=lightgray ctermfg=black guibg=lightgray guifg=black
:hi CursorColumn cterm=NONE ctermbg=lightgray ctermfg=lightgray guibg=lightgray guifg=lightgray
:let python_highlight_all=1
:hi statement ctermfg=white
:hi keyword ctermfg=white
:hi comment ctermfg=white
:hi function ctermfg=white
:hi identifier ctermfg=white
:hi type ctermfg=white
:hi tag ctermfg=white
:hi Function ctermfg=white
:hi Comment ctermfg=white
:hi Identifier ctermfg=white
:hi pythonStatement ctermfg=white
:hi pythonOperator ctermfg=white
:hi pythonConditional ctermfg=white
:hi pythonRepeat ctermfg=white
:hi vimHighCtermFgBg ctermfg=white
:hi vimSynType ctermfg=white
:hi LineNr ctermfg=white
:hi Number ctermfg=green
highlight Constant ctermfg=green

:nnoremap <leader>d dd
:set showcmd

:nnoremap <F5> :buffers<CR>:buffer<Space>

map <Leader>] :NERDTreeToggle


":colorscheme default
:set autoread
set statusline+=%#warningmsg#
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

set number
syntax enable
set background=dark
let g:solarized_termcolors = 256  " New line!!
let g:solarized_termtrans = 1  " New line!!
"colorscheme solarized
"colorscheme DimSlate
"colorscheme zellner
"colorscheme LightGreen
set hlsearch

vnoremap // y/<C-R>"<CR>
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]
set complete=.,b,u,]
set wildmode=longest,list:longest

let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

let $Tlist_Ctags_Cmd='/bin/ctags'

map T :TaskList<CR>
map P :TlistToggle<CR>

filetype plugin on
set omnifunc=syntaxcomplete#Complete

let python_highlight_all = 1

" NERD_tree config
"
"
let NERDTreeChDirMode=2
let NERDTreeIgnore=['\.vim$', '\~$', '\.pyc$', '\.swp$']
let NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$',  '\~$']
let NERDTreeShowBookmarks=1
map <F3> :NERDTreeToggle<CR>

" Syntax for multiple tag files are
"

" TagList Plugin Configuration
"
let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Close_On_Select = 1
let Tlist_Use_Right_Window = 1
let Tlist_File_Fold_Auto_Close = 1

map <F7> :TlistToggle<CR>

" Viewport Controls
" ie moving between split panes
"
map <silent>,h <C-w>h
map <silent>,j <C-w>j
map <silent>,k <C-w>k
map <silent>,l <C-w>l

"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1

let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:pymod_rope_lookup_project = 0
let g:pymod_rope = 0
let g:pymod_rope_autoimport = 0

syntax on
filetype plugin indent on
let g:paredit_leader = '\' 

:map <F5> :setlocal spell! spelllang=en_us<CR>
set laststatus=2
set statusline+=%F

set autoread
let g:pymode_rope = 0

"let g:syntastic_python_checkers=['flake8']
"let g:syntastic_python_flake8_args='--ignore=E501,E225,E702,E401,E302,W391,E271,E231,E251,E211'


map <F11> :PyflakesToggle<cr>
let g:pymode_folding=0
let g:pymode_warnings = 1
set hidden

let g:ackprg = 'ag --vimgrep'

set pastetoggle=<leader>p

set autochdir
autocmd FileType plaintex,tex,latex syntax spell toplevel

set statusline=%b\ %B
set shiftwidth=4
set tabstop=4
set expandtab

noremap // y/<C-R>"<CR>

set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

nnoremap <leader>el :ElmEvalLine<CR>
vnoremap <leader>es :<C-u>ElmEvalSelection<CR>
nnoremap <leader>em :ElmMakeCurrentFile<CR>

let g:paredit_leader = '\' 


" Setup some default ignores
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}

" Use the nearest .git directory as the cwd
" This makes a lot of sense if you are working on a project that is in version
" control. It also supports works with .svn, .hg, .bzr.
let g:ctrlp_working_path_mode = 'r'

" Use a leader instead of the actual named binding
 
"nmap <leader>p :CtrlP<cr>
nmap <leader>bb :CtrlPBuffer<cr>
nmap <leader>bm :CtrlPMixed<cr>
nmap <leader>bs :CtrlPMRU<cr>

" Use the right side of the screen
let g:buffergator_viewport_split_policy = 'R'

" I want my own keymappings...
let g:buffergator_suppress_keymaps = 1

" Looper buffers
"let g:buffergator_mru_cycle_loop = 1

" Go to the previous buffer open
nmap <leader>jj :BuffergatorMruCyclePrev<cr>

" Go to the next buffer open
nmap <leader>kk :BuffergatorMruCycleNext<cr>

" View the entire list of buffers open
nmap <leader>bl :BuffergatorOpen<cr>

" Shared bindings from Solution #1 from earlier
nmap <leader>T :enew<cr>
nmap <leader>bq :bp <BAR> bd #<cr>

set nocompatible
filetype off

set  rtp+=/usr/lib/python3.5/site-packages/powerline/bindings/vim/
set laststatus=1
set t_Co=256

syntax on
let clj_highlight_builtins = 1 "for clojure
let g:rainbow_active = 1
let g:rainbow_operators = 1

let g:ycm_filetype_blacklist = {}
let g:ycm_semantic_triggers = {'text': ["."]}
let g:ycm_semantic_triggers.clojure = [' ']
let g:ycm_semantic_triggers.clojure = ['rel!.']
"set omnifunc=your_clojure_complete_function
let g:ycm_path_to_python_interpreter = '/usr/bin/python'
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py'


let g:tex_flavor='latex'
set shellslash
set grepprg=grep\ -nH\ $*
"set fdm=indent
set fdm=syntax

let g:terraform_align=1
let g:terrafrom_fold_sections=1
let g:terrafrom_remap_spacebar=1
autocmd FileType terrafrom setlocal commentstring=#%s

filetype plugin on
filetype indent on

let g:tex_flavor='latex'

let g:pymode = 1
let g:pymode_rope = 0
let g:pymode_lint = 0

let g:pep8_map='<leader>8' 
command! WipeReg for i in range(34,122) | silent! call setreg(nr2char(i), []) | endfor

let g:vimtex_disable_version_warning = 1
let g:vimtex_compiler_latexmk = {'callback' : 0}
let g:vimtex_view_method = 'zathura'

autocmd BufWinEnter * silent! :%foldopen!

let g:python_recommended_style = 0
set statusline+=%F
set ts=4 shiftwidth=4 expandtab

" Ultisnips
let g:UltiSnipsExpandTrigger="<c-s>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsEditSplit="horizontal"
let g:UltiSnipsListSnippets="<c-l>"
let g:UltiSnipsSnippetDirectories = ['/Users/john/.vim/UltiSnips', 'UltiSnips']

set encoding=utf-8

smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

imap <expr><TAB>
	 \ neosnippet#expandable_or_jumpable() ?
	 \    "\<Plug>(neosnippet_expand_or_jump)" :
         \ 	  pumvisible() ? "\<C-n>" : "\<TAB>"

let g:deoplete#enable_at_startup = 1

let g:NERDTreeDirArrows = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
let g:NERDTreeGlyphReadOnly = "RO"
let g:NERDTreeNodeDelimiter = "\u00a0"

" Set no max file limit
let g:ctrlp_max_files = 0
" Search from current directory instead of project root
let g:ctrlp_working_path_mode = 0

" Ignore these directories
set wildignore+=*/out/**
set wildignore+=*/vendor/**

" Search in certain directories a large project (hardcoded for now)
cnoremap %proj <c-r>=expand('~/Projects/some-project')<cr>
" ga = go api
map <Leader>ga :CtrlP %proj/api/<cr>
" gf = go frontend
map <Leader>gf :CtrlP %proj/some/long/path/to/frontend/code/<cr>

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

" " Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" " Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P 

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

let g:conoline_auto_enable = 1
let g:indentLine_char_list = ['|', '¦', '┆', '┊']

"set cursorcolumn
"set cursorline

:highlight Cursor ctermfg=White ctermbg=Yellow cterm=bold guifg=white guibg=yellow gui=bold
:highlight CursorColumn ctermfg=White ctermbg=darkgray cterm=bold guifg=white guibg=yellow gui=bold
":highlight CursorLine ctermfg=White ctermbg=darkgray cterm=bold guifg=white guibg=#3E3D32 gui=bold

:set statusline=%<%f\ %h%m%r\ %y%=%{v:register}\ %-14.(%l,%c%V%)\ %P
set laststatus=2

