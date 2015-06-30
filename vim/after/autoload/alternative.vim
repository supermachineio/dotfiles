" Create a non-existant alternate file in a vertical split using a custom
" script.
function alternative#Alternative()
  :let alternative_path=system('~/Dropbox/developer_stuff/bin/alternative ' . expand('%:p'))
  :vs
  :e `=alternative_path`
endfunction
