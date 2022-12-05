{ pkgs }:

pkgs.tv.vim.makePlugin (pkgs.write "vim-tv" {
  #
  # Haskell
  #
  "/ftplugin/haskell.vim".text = ''
    if exists("g:vim_tv_ftplugin_haskell_loaded")
      finish
    endif
    let g:vim_tv_ftplugin_haskell_loaded = 1

    setlocal iskeyword+='
  '';
  #
  # TODO
  #
  "/ftdetect/todo.vim".text = ''
    au BufRead,BufNewFile TODO set ft=todo
  '';
  "/ftplugin/todo.vim".text = ''
    setlocal foldmethod=syntax
  '';
  "/syntax/todo.vim".text = ''
    syn match todoComment /#.*/

    syn match todoDate /^[1-9]\S*/
      \ nextgroup=todoSummary

    syn region todoSummary
      \ contained
      \ contains=todoTag
      \ start="." end="$\n"
      \ nextgroup=todoBlock

    syn match todoTag /\[[A-Za-z]\+\]/hs=s+1,he=e-1
      \ contained

    syn region todoBlock
      \ contained
      \ contains=Comment
      \ fold
      \ start="^[^1-9]" end="^[1-9]"re=s-1,he=s-1,me=s-1

    syn sync minlines=1000

    hi link todoComment Comment
    hi todoDate ctermfg=255
    hi todoSummary ctermfg=229
    hi todoBlock ctermfg=248
    hi todoTag ctermfg=217
  '';
})
