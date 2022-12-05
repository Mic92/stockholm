with import <stockholm/lib>;
{ pkgs }:

pkgs.tv.vim.makePlugin (pkgs.write "vim-syntax-nix-nested" {
  "/syntax/haskell.vim".text = ''
    syn region String start=+\[[[:alnum:]]*|+ end=+|]+

    hi link ConId Identifier
    hi link VarId Identifier
    hi link hsDelimiter Delimiter
  '';
  "/syntax/nix.vim".text = ''
    "" Quit when a (custom) syntax file was already loaded
    "if exists("b:current_syntax")
    "  finish
    "endif

    "setf nix

    " Ref <nix/src/libexpr/lexer.l>
    syn match NixID    /[a-zA-Z\_][a-zA-Z0-9\_\'\-]*/
    syn match NixINT   /\<[0-9]\+\>/
    syn match NixPATH  /[a-zA-Z0-9\.\_\-\+]*\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
    syn match NixHPATH /\~\(\/[a-zA-Z0-9\.\_\-\+]\+\)\+/
    syn match NixSPATH /<[a-zA-Z0-9\.\_\-\+]\+\(\/[a-zA-Z0-9\.\_\-\+]\+\)*>/
    syn match NixURI   /[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']\+/
    syn region NixSTRING
      \ matchgroup=NixSTRING
      \ start='"'
      \ skip='\\"'
      \ end='"'
    syn region NixIND_STRING
      \ matchgroup=NixIND_STRING
      \ start="'''"
      \ skip="'''\('\|[$]\|\\[nrt]\)"
      \ end="'''"

    syn match NixOther /[-!+&<>|():/;=.,?\[\]*@]/

    syn match NixCommentMatch /\(^\|\s\)#.*/
    syn region NixCommentRegion start="/\*" end="\*/"

    hi link NixCode Statement
    hi link NixData Constant
    hi link NixComment Comment

    hi link NixCommentMatch NixComment
    hi link NixCommentRegion NixComment
    hi link NixID NixCode
    hi link NixINT NixData
    hi link NixPATH NixData
    hi link NixHPATH NixData
    hi link NixSPATH NixData
    hi link NixURI NixData
    hi link NixSTRING NixData
    hi link NixIND_STRING NixData

    hi link NixEnter NixCode
    hi link NixOther NixCode
    hi link NixQuote NixData

    syn cluster nix_has_dollar_curly contains=@nix_ind_strings,@nix_strings
    syn cluster nix_ind_strings contains=NixIND_STRING
    syn cluster nix_strings contains=NixSTRING

    ${concatStringsSep "\n" (let
      alts = xs: ''\(${concatStringsSep ''\|'' xs}\)'';
      capitalize = s: let
        xs = stringToCharacters s;
      in
        toUpper (head xs) + concatStrings (tail xs);
      comment = k: ''/\* ${k} \*/'';
      def = k: ''${k}[ \t\r\n]*='';
      writer = k: ''write${k}[^ \t\r\n]*[ \t\r\n]*\("[^"]*"\|[a-z]\+\)'';
      writerExt = k: writerName ''[^"]*\.${k}'';
      writerName = k:
        ''${alts [''toFile'' ''write[^ \t\r\n]*'']}*[ \t\r\n]*"${k}"'';
    in mapAttrsToList (name: {
      extraStart ? null,
      lang ? name
    }:
    let
      startAlts = filter isString [
        (comment name)
        extraStart
      ];
      sigil = ''${alts startAlts}[ \t\r\n]*'';
    in /* vim */ ''
      syn include @nix_${lang}_syntax syntax/${lang}.vim
      if exists("b:current_syntax")
        unlet b:current_syntax
      endif

      syn match nix_${lang}_sigil
        \ X${replaceStrings ["X"] ["\\X"] sigil}\ze\('''\|"\)X
        \ nextgroup=nix_${lang}_region_IND_STRING,nix_${lang}_region_STRING
        \ transparent

      syn region nix_${lang}_region_STRING
        \ matchgroup=NixSTRING
        \ start='"'
        \ skip='\\"'
        \ end='"'
        \ contained
        \ contains=@nix_${lang}_syntax
        \ transparent

      syn region nix_${lang}_region_IND_STRING
        \ matchgroup=NixIND_STRING
        \ start="'''"
        \ skip="'''\('\|[$]\|\\[nrt]\)"
        \ end="'''"
        \ contained
        \ contains=@nix_${lang}_syntax
        \ transparent

      syn cluster nix_ind_strings
        \ add=nix_${lang}_region_IND_STRING

      syn cluster nix_strings
        \ add=nix_${lang}_region_STRING

      " This is required because containedin isn't transitive.
      syn cluster nix_has_dollar_curly
        \ add=@nix_${lang}_syntax
    '') {
      c = {};
      cabal = {};
      diff = {};
      exim = {};
      haskell = {};
      jq.extraStart = alts [
        (writer "Jq")
        (writerExt "jq")
      ];
      javascript.extraStart = comment "js";
      lua = {};
      markdown.extraStart = writerExt "md";
      nftables = {};
      #nginx = {};
      python.extraStart = alts [
        (comment "py")
        (writerExt "py")
      ];
      sed.extraStart = writer "Sed";
      sh.extraStart = let
        phases = [
          "unpack"
          "patch"
          "configure"
          "build"
          "check"
          "install"
          "fixup"
          "installCheck"
          "dist"
        ];
        shells = [
          "ash"
          "bash"
          "dash"
        ];
      in alts [
        (def "shellHook")
        (def "${alts phases}Phase")
        (def "${alts ["pre" "post"]}${alts (map capitalize phases)}")
        (writer (alts (map capitalize shells)))
      ];
      yaml = {};
      vim.extraStart = alts [
        (def ''"[^"]*\.vim"\.text'')
        (writerExt "vim")
        (writerName ''\([^"]*\.\)\?vimrc'')
      ];
      xdefaults = {};
      xmodmap = {};
    })}

    " Clear syntax that interferes with nixINSIDE_DOLLAR_CURLY.
    syn clear shVarAssign

    syn region nixINSIDE_DOLLAR_CURLY
      \ matchgroup=NixEnter
      \ start="[$]{"
      \ end="}"
      \ contains=TOP
      \ containedin=@nix_has_dollar_curly
      \ transparent

    syn region nix_inside_curly
      \ matchgroup=NixEnter
      \ start="{"
      \ end="}"
      \ contains=TOP
      \ containedin=nixINSIDE_DOLLAR_CURLY,nix_inside_curly
      \ transparent

    syn match NixQuote /'''\(''$\|\\.\)/he=s+2
      \ containedin=@nix_ind_strings
      \ contained

    syn match NixQuote /'''\('\|\\.\)/he=s+1
      \ containedin=@nix_ind_strings
      \ contained

    syn match NixQuote /\\./he=s+1
      \ containedin=@nix_strings
      \ contained

    syn sync fromstart

    let b:current_syntax = "nix"

    set isk=@,48-57,_,192-255,-,'
  '';
  "/syntax/sed.vim".text = ''
    syn region sedBranch
      \ matchgroup=sedFunction start="T"
      \ matchgroup=sedSemicolon end=";\|$"
      \ contains=sedWhitespace
  '';
  "/syntax/xmodmap.vim".text = ''
    syn match xmodmapComment /^\s*!.*/
  '';
})
