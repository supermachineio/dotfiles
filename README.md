# Gust Dotfiles

## About
Dotfile repository for Gust development machines. Currently only supports vim, but may eventually be used to manage others (i.e. .bashrc, .gitconfig, etc.).

## Quickstart

### Install
1. Backup any existing dot files in /Users/home (.vimrc, .vim, .gvimrc)
2. Clone the repository (i.e. ~/dotfiles)
3. Navigate to the folder where the repository was cloned
4. Run the `update_symlinks.sh` script.
5. Install vim plugins with `vim +PluginInstall! +qall`
6. Install the CtrlP C extension:
```bash
cd ~/.vim/bundle/ctrlp-cmatcher
./install.sh
```

## Overview

### TODO

* setup gvimrc file with the <D-> mappings that are disabled in terminal vim

### Vim

#### New Plugins

##### Search for something, then write the original files directly through the search results 
'AndrewRadev/writable_search.vim'

##### Allows you to save files into directories that do not exist yet
'DataWraith/auto_mkdir'

##### Cscope keyboard mappings
Plugin 'chazy/cscope_maps'

##### Makes source navigation of bundled gems easier
'tpope/vim-bundler'

##### Project configuration
'tpope/projectionist'

##### Makes Ruby project navigation easier for non-Rails projects
'tpope/vim-rake'

#### New Keymaps

The following new keymaps have been added.

##### Quickly switch to alternate file
nnoremap <Leader><Leader> <c-^>

##### Map ,e and ,v to open files in the same directory as the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%
map <leader>v :view %%

##### Apply macros w/ Q
nnoremap Q @q
vnoremap Q :norm @q<cr>

##### Regenerate ctags and cscope.out using starscope gem
map <F9> :StarscopeUpdate<cr>

##### Run test file
nmap <CR><CR> :wa <bar> TestifyRunFile<CR>

#### Deprecated Plugins
The following plugins were included in an older version of the vim configuration and are not included in the new config.

* Julian/vim-textobj-variable-segment
* Lokaltog/vim-easymotion
* arcWeber/vim-addon-mw-utils
* Peeja/vim-cdo
* YankRing.vim
* ZoomWin
* brysgo/quickfixfix
* brysgo/test_server
* camelcasemotion
* carlobaldassi/ConqueTerm
* ecomba/vim-ruby-refactoring
* godlygeek/tabular
* henrik/vim-qargs
* jgdavey/vim-blockle
* kana/vim-textobj-line
* kchmck/vim-coffee-script
* matt-royal/diffthese
* mgamba/j-split
* nono/vim-handlebars
* pivotal/tmux-config
* regreplop.vim
* rosstimson/scala-vim-support
* slim-template/vim-slim
* thinca/vim-textobj-between
* vim-scripts/L9

#### Deprecated Keymaps
The following keymaps are not included and are suggested for deprecation. Note that when vim is run in a terminal, the <command> key (<D->) cannot be mapped, so most of these mappings are deprecated for that reason.

##### Search
* nmap <leader>s  :%s/
* vmap <leader>s  :s/

##### Split screen
* map <leader>v   :vsp<CR>

##### Move between screens
* map <leader>w   ^Ww
* map <leader>=   ^W=
* map <leader>j   ^Wj
* map <leader>k   ^Wk

Note that the other mappings (<C-j>, <C-k>, etc.) are still available.

##### Open .vimrc file in new tab. Think Command + , [Preferences...] but with Shift.
* map <D-<>       :tabedit ~/.vimrc<CR>

##### Reload .vimrc
* map <leader>rv  :source ~/.vimrc<CR>

##### Undo/redo - Doesn't MacVim already have this?
* map <D-z>       :earlier 1<CR>
* map <D-Z>       :later 1<CR>

##### Auto-indent whole file
map <silent> <F7> gg=G`` :delmarks z<CR>:echo "Reformatted."<CR>

Note that <leader>= is still available.

##### Jump to a new line in insert mode
* imap <D-CR>     <Esc>o

##### Fast scrolling
* nnoremap <C-e>  3<C-e>
* nnoremap <C-y>  3<C-y>

##### Previous/next quickfix file listings (e.g. search results)
* map <M-D-Down>  :cn<CR>
* map <M-D-Up>    :cp<CR>

##### Previous/next buffers
* map <M-D-Left>  :bp<CR>
* map <M-D-Right> :bn<CR>

##### Git blame
* map <leader>g   :Gblame<CR>

##### Comment/uncomment lines
* map <D-/>       <plug>NERDCommenterToggle
* imap <D-/>      <Esc><plug>NERDCommenterToggle i

Note that <leader>/ is still available.

##### In command-line mode, <C-A> should go to the front of the line, as in bash.
* cmap <C-A> <C-B>

There is no need for this mapping since this is the default in terminal vim.

##### Copy current file path to system pasteboard
* map <silent> <D-C> :let @* = expand("%")<CR>:echo "Copied: ".expand("%")<CR>

Note that <leader>C is still available for this functionality.

##### Run tests
* map <leader>tt :wa<CR>:RunTestAgain<CR>
* map <F12> :write<CR>:RunTest<CR>
* imap <F12> <ESC><F12>
* map <F11> :write<CR>:RunTestLine<CR>
* imap <F11> <ESC><F11>
* map <F10> :write<CR>:RunTestAgain<CR>
* imap <F10> <ESC><F10>
* map <F9> :write<CR>:RunTestPrevious<CR>
* imap <F9> <ESC><F9>

Note that <leader>t (run a focused test) and <leader>T (run the test file) are still available, but use the "testify" plugin, which will run the tests using vim-dispatch and put the results in the quickfix window for easy navigation. <cr><cr> is also an alias for <leader>T. Since the "testify" plugin keeps track of the last test that was ran, the "RunTestAgain" and "RunTestPrevious" are no longer needed.

##### Disable middle mouse button, F1
* map <MiddleMouse>   <Nop>
* imap <MiddleMouse>  <Nop>
* map <F1>            <Nop>
* imap <F1>           <Nop>

##### Easy access to the shell
* map <Leader><Leader> :!

##### AckGrep
* map <leader>a :call AckGrep()<CR>
* vmap <leader>a :call AckVisual()<CR>

QUESTION: Since Ack is no longer used, should these mappings be used for Ag instead?

##### Recalculate diff when it gets messed up.
* nmap du :diffupdate<CR>

##### Gundo.vim
* map <leader>u :GundoToggle<CR>

##### ctrlp
* nnoremap <silent> <leader>F :CtrlPClearAllCaches<CR>:CtrlPCurWD<CR>
* nnoremap <silent> <leader>bb :CtrlPBuffer<cr>
* map <D-e> :CtrlPBuffer<CR>
* nnoremap <silent> <C-p> :CtrlPMRU<cr>
* nnoremap <silent> <D-P> :ClearCtrlPCache<cr>

The CtrlP plugin already has mappings for these. From within the CtrlP window use <c-f> and <c-b> for switching between file, MRU and buffer modes. <F5> will refresh the cache when the CtrlP window is open.

##### Open CtrlP starting from a particular path
* map <leader>jm :CtrlP app/models<CR>
* map <leader>jc :CtrlP app/controllers<CR>
* map <leader>jv :CtrlP app/views<CR>
* map <leader>jh :CtrlP app/helpers<CR>
* map <leader>jl :CtrlP lib<CR>
* map <leader>jp :CtrlP public<CR>
* map <leader>js :CtrlP spec<CR>
* map <leader>jf :CtrlP fast_spec<CR>
* map <leader>jd :CtrlP db<CR>
* map <leader>jC :CtrlP config<CR>
* map <leader>jV :CtrlP vendor<CR>
* map <leader>jF :CtrlP factories<CR>
* map <leader>jT :CtrlP test<CR>

These seem like they be be useful, but I'm not sure how often they're actually used.

##### Cmd-Shift-(M)ethod - jump to a method (tag in current file)
* nnoremap <silent> <D-M> :CtrlPBufTag<CR>

##### Mappings inherited from FuzzyFinder
* map <leader><C-N> :CtrlPCurWD<CR>
* map <leader>n :CtrlPCurWD<CR>
* map <D-N> :CtrlPCurWD<CR>

##### Write all
* map <silent> <F19>WriteAll :silent! wall<CR>

##### Tagbar
* nmap <leader>l :TagbarToggle<CR>

This has been remapped to <F-8>.

##### Cmd-Shift-F searches the whole project (like in TextMate, RubyMine, etc.)
* map <D-F> :Ag<Space>

##### YankRing show registers
* :nnoremap <silent> <F6> :YRShow<CR>

##### Convert a word to to let(:word) { double(:word) }
* nmap <leader>ld <Plug>LocalMakelet
