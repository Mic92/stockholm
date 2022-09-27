{ attr, coreutils, exiv2, findutils, gnugrep, jq, nix, stockholm, util-linux, stdenv }:
stdenv.mkDerivation rec {
  pname = "htgen-imgur";
  version = "1.0.0";

  src = ./src;

  buildPhase = ''
    (
      exec > htgen-imgur
      echo PATH=${stockholm.lib.makeBinPath [
        attr
        coreutils
        exiv2
        findutils
        gnugrep
        jq
        nix util-linux
      ]}
      echo STATEDIR=${stockholm.lib.shell.escape "\${STATEDIR-$HOME}"}
      cat $src/htgen-imgur
    )
  '';

  installPhase = ''
    install -D htgen-imgur $out/bin/htgen-imgur
  '';
}
