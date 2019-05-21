with import <stockholm/lib>;
{ pkgs }:

(rtp: rtp // { inherit rtp; }) (pkgs.writeTextFile (let
  name = "hack";
in {
  name = "vim-color-${name}-1.0.2";
  destination = "/colors/${name}.vim";
  text = /* vim */ ''
    set background=dark
    hi clear
    if exists("syntax_on")
      syntax clear
    endif

    let colors_name = ${toJSON name}

    hi Normal       ctermbg=235
    hi Comment      ctermfg=242
    hi Constant     ctermfg=255
    hi Identifier   ctermfg=253
    hi Function     ctermfg=253
    hi Statement    ctermfg=253
    hi PreProc      ctermfg=251
    hi Type         ctermfg=251
    hi Delimiter    ctermfg=251
    hi Special      ctermfg=255

    hi Garbage      ctermbg=088
    hi TabStop      ctermbg=016
    hi Todo         ctermfg=174 ctermbg=NONE

    hi NixCode      ctermfg=040
    hi NixData      ctermfg=046
    hi NixQuote     ctermfg=071

    hi diffNewFile  ctermfg=207
    hi diffFile     ctermfg=207
    hi diffLine     ctermfg=207
    hi diffSubname  ctermfg=207
    hi diffAdded    ctermfg=010
    hi diffRemoved  ctermfg=009

    hi Search       cterm=NONE ctermbg=216
  '';
}))
