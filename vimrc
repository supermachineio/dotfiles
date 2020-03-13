"#############################################################################
" Install / load plugins
"#############################################################################

" Required for Vundle
filetype off

let need_to_install_plugins=0
let base16colorspace=256
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
let g:ctrlp_user_command = {
    \ 'types': {
    \   1: ['.git', 'cd %s && git ls-files --cached --exclude-standard --others']
    \ },
    \ 'fallback': 'ag %s -l --nocolor -g ""' }

" Bootstrap Vundle if it's not installed
if empty(system("grep lazy_load ~/.vim/bundle/Vundle.vim/autoload/vundle.vim"))
    silent !mkdir -p ~/.vim/bundle
    silent !rm -rf ~/.vim/bundle/Vundle.vim
    silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/Vundle.vim
    let need_to_install_plugins=1
endif

if &term =~ '256color'
    " Disable Background Color Erase (BCE) so that color schemes
    " work properly when Vim is used inside tmux and GNU screen.
    set t_ut=
endif

set runtimepath+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/Vundle.vim'                          " let Vundle manage Vundle, required

Plugin 'AndrewRadev/writable_search.vim'            " Grep for something, then write the original files directly through the search results
Plugin 'Blackrush/vim-gocode'                       " Golang bundle
Plugin 'DataWraith/auto_mkdir'                      " Allows you to save files into directories that do not exist yet
Plugin 'MarcWeber/vim-addon-mw-utils'               " vim-snipmate dependency
Plugin 'airblade/vim-gitgutter'                     " shows a git diff in the gutter (sign column) and stages/reverts hunks
Plugin 'altercation/vim-colors-solarized'           " Solarized color theme
" Plugin 'challenger-deep-theme/vim', { 'name': 'challenger-deep' }
Plugin 'dracula/vim', { 'name': 'dracula' }
Plugin 'bling/vim-airline'                          " lean & mean status/tabline for vim that's light as air
" Plugin 'itchyny/lightline.vim'
Plugin 'chazy/cscope_maps'                          " cscope keyboard mappings
Plugin 'chrisbra/csv.vim'                           " Filetype plugin for csv files
Plugin 'ervandew/supertab'                          " Perform all your vim insert mode completions with Tab
Plugin 'garbas/vim-snipmate'                        " handy code snippets
Plugin 'godlygeek/csapprox'                         " dependency for Solarized
Plugin 'godlygeek/tabular'                          " dependency of vim-markdown
Plugin 'honza/vim-snippets'                         " vim-snipmate default snippets
Plugin 'kana/vim-textobj-user'                      " dependency for rubyblock
Plugin 'ctrlpvim/ctrlp.vim'                             " Fuzzy file, buffer, mru, tag, etc finder
Plugin 'majutsushi/tagbar'                          " displays tags in a window, ordered by scope
Plugin 'mustache/vim-mustache-handlebars'           " mustache and handlebars mode
Plugin 'nelstrom/vim-textobj-rubyblock'             " custom text object for selecting Ruby blocks
Plugin 'pangloss/vim-javascript'                    " Vastly improved Javascript indentation and syntax support
Plugin 'plasticboy/vim-markdown'                    " markdown support; requires godlygeek/tabular
Plugin 'rking/ag.vim'                               " plugin for the_silver_searcher
Plugin 'scrooloose/nerdcommenter'                   " quickly (un)comment lines
Plugin 'scrooloose/nerdtree'                        " A tree explorer plugin
Plugin 'sjl/vitality.vim'                           " Make Vim play nicely with iTerm 2 and tmux
Plugin 'tomtom/tlib_vim'                            " vim-snipmate dependency
Plugin 'tpope/vim-abolish'                          " easily search for, substitute, and abbreviate multiple variants of a word
Plugin 'tpope/vim-bundler'                          " makes source navigation of bundled gems easier
Plugin 'tpope/vim-cucumber'                         " provides syntax highlightling, indenting, and a filetype plugin
Plugin 'tpope/vim-dispatch'                         " Asynchronous build and test dispatcher
Plugin 'tpope/vim-endwise'                          " wisely add 'end' in ruby, endfunction/endif/more in vim script, etc
Plugin 'tpope/vim-fugitive'                         " Git plugin
Plugin 'tpope/vim-haml'                             " HAML support
Plugin 'tpope/vim-projectionist'                    " project configuration
Plugin 'tpope/vim-rails'                            " Rails helpers
Plugin 'tpope/vim-rake'                             " makes Ruby project navigation easier for non-Rails projects
Plugin 'tpope/vim-repeat'                           " Enable repeating supported plugin maps with '.'
Plugin 'tpope/vim-surround'                         " makes working w/ quotes, braces,etc. easier
Plugin 'tpope/vim-unimpaired'                       " pairs of handy bracket mappings
Plugin 'vim-ruby/vim-ruby'                          " packaged w/ vim but this is latest and greatest
Plugin 'vim-scripts/regreplop.vim'                  " operator to replace motion/visual with a register
Plugin 'vim-scripts/vim-auto-save'                  " automatically save changes to disk
Plugin 'elmcast/elm-vim'                            " elm syntax highlighting and utilities
Plugin 'lmeijvogel/vim-yaml-helper'                 " navigate yaml files more easily
Plugin 'aperezdc/vim-template'                      " templates by file type
Plugin 'leafgarland/typescript-vim'                 " Typescript syntax
Plugin 'Quramy/tsuquyomi'                           " Typescript tools
Plugin 'Shougo/vimproc'                             " Async command processor
Plugin 'kchmck/vim-coffee-script'                   " Coffeescript plugin
Plugin 'digitaltoad/vim-pug'                        " Jade/Pug syntax highlighting, indenting, and filetype
Plugin 'FelikZ/ctrlp-py-matcher'
Plugin 'hashivim/vim-terraform'

call vundle#end()

if 1 == need_to_install_plugins
    silent! PluginInstall
    q
endif

"#############################################################################
" Settings
"#############################################################################
set autoread                                    " Detect file changes refresh buffer
set background=dark                             " Dark colored background
set backspace=indent,eol,start                  " Backspace of newlines
set colorcolumn=79                              " Show vertical column
set cursorline                                  " Highlight current line
set expandtab                                   " Expand tabs to spaces
set formatoptions=qrn1                          " http://vimdoc.sourceforge.net/htmldoc/change.html#fo-table
set hidden                                      " allow buffers to be hidden
set history=1024                                " History size
set hlsearch                                    " Highlight matches to recent searches
set ignorecase                                  " Ignore case when searching
set incsearch                                   " Use incremental search
set laststatus=2                                " Use two rows for status line
set list                                        " Show invisible chars
set listchars=tab:»·,trail:·                    " Show tabs and trailing whitespace only
set nocompatible                                " Not compatible w/ vi
set number                                      " Display line numbers
set ruler                                       " Show line and column number of cursor
set scrolloff=3                                 " Always show 3 lines around cursor
set splitright                                  " open new vertical buffers on the right...
set splitbelow                                  " ...and horizontal ones below
set showmatch                                   " Show matching braces
set smartcase                                   " Turn case sensitive search back on in certain cases
set sw=2 sts=2 ts=2                             " 2 spaces
set swapfile                                    " Keep swapfiles
set directory=~/.vim-tmp,~/tmp,/var/tmp,/tmp
set backupdir=~/.vim-tmp,~/tmp,/var/tmp,/tmp
set t_Co=256                                    " Use 256 colors
set textwidth=0                                 " Do not break lines
set ttimeoutlen=100                             " Without this entering normal mode takes forever
set wildmenu                                    " Autocomplete filenames
set wildmode=list:longest,full                  " Show completions as list with longest match then full matches
set wildignore+=tags                            " Ignore certain files/folders when globbing
set wildignore+=cscope.out
set wildignore+=tmp/**
set wildignore+=public/uploads/**
set wildignore+=public/images/**
set wildignore+=vendor/**
set nowrap                                        " Turn off line wrapping

"#############################################################################
" Misc
"#############################################################################

" Enable bundled matchit macros
runtime macros/matchit.vim

filetype on
filetype plugin on
filetype plugin indent on
syntax on
let mapleader = ","
let maplocalleader = ";"

"#############################################################################
" Plugin configuration
"#############################################################################
if has('nvim') || has('termguicolors')
  set termguicolors
endif

let g:airline_powerline_fonts = 1
" let g:lightline = { 'colorscheme': 'challenger_deep'}

let NERDSpaceDelims = 1

let ruby_operators=1

let g:solarized_termcolors=256
let g:solarized_visibility="high"
let g:solarized_contrast="high"


let g:testify_launcher = "Dispatch "

" Override the testify launchers in ./vim/after/plugin/testify.vim
let g:testify_runners = {
            \ 'cucumber': 'zeus cucumber ',
            \ 'rspec': 'zeus rspec ',
            \ 'python': ' ./venv/bin/python -m unittest ' }

let g:auto_save = 0
let g:auto_save_no_updatetime = 1
let g:auto_save_in_insert_mode = 0

let g:templates_directory = "~/.vim/templates/"

let g:templates_user_variables = [
  \   ['GIT_USER', 'GetGitUser'],
  \ ]
function! GetGitUser()
    execute('r!/usr/bin/git config user.name')
    execute("s/^/# Author: /")
    execute('r!/usr/bin/git config user.email')
    execute("-1" . "normal J")
endfunction

"#############################################################################
" Keymaps
"#############################################################################

" Gracefully handle holding shift too long after : for common commands
cabbrev W w
cabbrev Q q
cabbrev Wq wq
cabbrev Tabe tabe
cabbrev Tabc tabc

" Make Y consistent with D and C
map Y y$

" File tree browser
map \ :NERDTreeToggle<CR>

" File tree browser showing current file - pipe (shift-backslash)
map \| :NERDTreeFind<CR>

" Search using Ag
noremap ,a :Ag<CR>

" Ctrl-P Mapping
nnoremap <Leader>f :CtrlP<cr>

" split pane shortcut
nnoremap <Leader>v :vs<cr>

" Open Fugitive status buffer
nnoremap <Leader>g :Gstatus<CR>

" Open and close the quickfix window
map <leader>qo :copen<CR>
map <leader>qc :cclose<CR>

"indent/unindent visual mode selection with tab/shift+tab
vmap <tab> >gv
vmap <s-tab> <gv

" ctags again with gemhome added
map <leader>rt :!/usr/local/bin/ctags -R --exclude=.git --exclude=log * `rvm gemhome`/*<CR>
map <leader>rT :!rdoc -f tags -o tags * `rvm gemhome` --exclude=.git --exclude=log

" Comment/uncomment lines
map <leader>/ <plug>NERDCommenterToggle

" Copy current file path to system pasteboard
map <leader>C :let @* = expand("%").":".line(".")<CR>:echo "Copied: ".expand("%").":".line(".")<CR>

" Delete focused buffer without losing split
nnoremap <C-c> :bp\|bd #<CR>

" Press Space to turn off highlighting and clear any message already
" displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>""

" Quickly switch to alternate file
nnoremap <Leader><Leader> <c-^>

" Auto-indent whole file
nmap <leader>=  gg=G``

" Map ,e and to open files in the same directory as the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%

" Map ,s to search and replace"
noremap <leader>s :%s/

" Make it easier to switch between windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Apply macros w/ Q
nnoremap Q @q
vnoremap Q :norm @q<cr>

" Run tests
map <leader>t :wa <bar> TestifyRunFocused<CR>
nmap <leader>T :wa <bar> TestifyRunFile<CR>
nmap <CR><CR> :wa <bar> TestifyRunFile<CR>

" Toggle paste/nopaste mode
map <F2> :set paste!<CR>

" Toggle TagBar
map <F8> :TagbarToggle<CR>

" Regenerate ctags and cscope.out using starscope gem
map <F9> :StarscopeUpdate<cr>

" Call the 'alternative' script
nnoremap <Leader>A :Alternative<CR>

"Copy yml key under the cursor"
nnoremap <Leader>k :call YMLToKey()<CR>
function YMLToKey()
  execute '!~/Dropbox/developer_stuff/bin/yml_to_key key ' . expand('%:p') . ":" . line(".") . ' | xargs echo -n | pbcopy'
endfunction

"#############################################################################
" Autocommands
"#############################################################################

" Strip trailing whitespace for code files on save
function! StripTrailingWhitespace()
  let save_cursor = getpos(".")
  %s/\s\+$//e
  call setpos('.', save_cursor)
endfunction
autocmd BufWritePre *.rb,*.yml,*.js,*.css,*.less,*.sass,*.scss,*.html,*.xml,*.erb,*.haml,*.feature call StripTrailingWhitespace()

" Highlight Ruby files
au BufRead,BufNewFile *.thor set filetype=ruby
au BufRead,BufNewFile *.god set filetype=ruby
au BufRead,BufNewFile Gemfile* set filetype=ruby
au BufRead,BufNewFile Vagrantfile set filetype=ruby
au BufRead,BufNewFile soloistrc set filetype=ruby
au BufRead,BufNewFile *_spec.rb set syntax=ruby

" Highlight JSON files as javascript
autocmd BufRead,BufNewFile *.json set filetype=javascript

" Highlight Jasmine fixture files as HTML
autocmd BufRead,BufNewFile *.jasmine_fixture set filetype=html

" When viewing a git tree or blob, quickly move up to view parent
autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

" Auto-clean fugitive buffers
autocmd BufReadPost fugitive://* set bufhidden=delete

" Word wrap without line breaks for text files
au BufRead,BufNewFile *.txt,*.md,*.markdown,*.rdoc set wrap linebreak nolist textwidth=0 wrapmargin=0

colorscheme solarized
" colorscheme challenger_deep
" colorscheme dracula
