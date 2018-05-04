{ stdenv, pkgs, ... }:
let
  py = pkgs.python3Packages.python.withPackages (p: [
    p.werkzeug
    p.beautifulsoup4
  ]);
  src = pkgs.fetchFromGitHub {
    owner = "k4cg";
    repo = "nichtparasoup";
    rev = "cf164b5";
    sha256 = "09bwh76agp14j8rv7bp47jcwhffc1b0bak0ikvzxyphph5lyidk9";
  };
  patchedSrc = stdenv.mkDerivation {
    name = "nichtparasoup";
    inherit src;
    patches = [ ./exception.patch ];
    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -r * $out/
    '';
  };
in pkgs.writeDashBin "nichtparasoup" ''
  ${py}/bin/python ${patchedSrc}/nichtparasoup.py "$@"
''
