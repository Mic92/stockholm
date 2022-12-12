with import ./lib;
{ pkgs }:

pkgs.tv.vim.makePlugin (pkgs.writeTextFile (let
  name = "vim";
in {
  name = "vim-syntax-${name}-1.0.0";
  destination = "/syntax/${name}.vim";
  text = /* vim */ ''
    ${concatMapStringsSep "\n" (s: /* vim */ ''
      syn keyword vimColor${s} ${s}
        \ containedin=ALLBUT,vimComment,vimLineComment
      hi vimColor${s} ctermfg=${s}
    '') (map (i: lpad 3 "0" (toString i)) (range 0 255))}
  '';
}))
