{stdenv, fetchurl,pkgs,... }:
let
  s =
  rec {
    baseName="translate-shell";
    version="0.9.0.9";
    name="${baseName}-${version}";
    url=https://github.com/soimort/translate-shell/archive/v0.9.0.9.tar.gz;
    sha256="1269j4yr9dr1d8c5kmysbzfplbgdg8apqnzs5w57d29sd7gz2i34";
  };
  searchpath = with pkgs; stdenv.lib.makeSearchPath "bin" [
    fribidi
    gawk
    bash
    curl
    less
  ];
  buildInputs = [
    pkgs.makeWrapper
  ];
in
stdenv.mkDerivation {
  inherit (s) name version;
  inherit buildInputs;
  src = fetchurl {
    inherit (s) url sha256;
  };
  # TODO: maybe mplayer
  installPhase = ''
    mkdir -p $out/bin
    make PREFIX=$out install
    wrapProgram $out/bin/trans --suffix PATH : "${searchpath}"
  '';

  meta = {
    inherit (s) version;
    description = ''translate using google api'';
    license = stdenv.lib.licenses.free;
    maintainers = [stdenv.lib.maintainers.makefu];
    platforms = stdenv.lib.platforms.linux ;
  };
}

