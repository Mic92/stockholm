{ pkgs }:

pkgs.tv.vim.makePlugin (pkgs.writeTextFile (let
  name = "showsyntax";
in {
  name = "vim-plugin-${name}-1.0.0";
  destination = "/plugin/${name}.vim";
  text = /* vim */ ''
    if exists('g:loaded_showsyntax')
      finish
    endif
    let g:loaded_showsyntax = 0

    fu! ShowSyntax()
      let id = synID(line("."), col("."), 1)
      let name = synIDattr(id, "name")
      let transName = synIDattr(synIDtrans(id),"name")
      if name != transName
        let name .= " (" . transName . ")"
      endif
      echo "Syntax: " . name
    endfu

    command! -n=0 -bar ShowSyntax :call ShowSyntax()
  '';
}))
