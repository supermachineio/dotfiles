# Supermachine Dotfiles

## About
Dotfile repository for Supermachine development machines. Currently only supports vim, but may eventually be used to manage others (i.e. .bashrc, .gitconfig, etc.).

## Usage
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

### Update

## Overview

### Contributing

Use [GitHub issues](https://github.com/supermachineio/dotfiles/issues) for reporting bug and feature requests.

### Vim

Plugins are manage using [Vundle](https://github.com/gmarik/Vundle.vim). Most of the plugins from the old vim config were ported over to the new configuration. Any new plugins or ones that were *not* ported over are listed below.

#### New Plugins

* [AndrewRadev/writable_search.vim](https://github.com/AndrewRadev/writable_search.vim) - Search for something, then write the original files directly through the search results 
* [DataWraith/auto_mkdir](https://github.com/DataWraith/auto_mkdir) - Allows you to save files into directories that do not exist yet
* [chazy/cscope_maps](https://github.com/chazy/cscope_maps) - Cscope keyboard mappings
* [tpope/vim-bundler](https://github.com/tpope/vim-bundler) - Makes source navigation of bundled gems easier
* [tpope/projectionist](https://github.com/tpope/vim-projectionist) - Project configuration
* [tpope/vim-rake](https://github.com/tpope/vim-rake) - Makes Ruby project navigation easier for non-Rails projects

#### New Keymaps

The following new keymaps have been added.

Keymap | Description
---|---
`nnoremap <Leader><Leader> <c-^>`             | Quickly switch to alternate file
`cnoremap %% <C-R>=expand('%:h').'/'<cr>`     | Expand %% to current open buffer directory
`map <leader>e :edit %%`                      | Quickly edit a file in the current buffer's directory
`map <leader>v :view %%`                      | Quickly view a file in the current buffer's directory
`nnoremap Q @q`                               | Apply macros w/ Q
`vnoremap Q :norm @q<cr>`                     |
`map <F9> :StarscopeUpdate<cr>`               | Regenerate ctags and cscope.out using starscope gem
`nmap <CR><CR> :wa <bar> TestifyRunFile<CR>`  | Run test file
