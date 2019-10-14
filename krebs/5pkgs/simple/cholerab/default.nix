{ stdenv, fetchFromGitHub, pandoc }:
stdenv.mkDerivation {
  name = "cholerab";
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "cholerab";
    rev = "25d7ef051d6fc74d99b155e768b3c650296a230c";
    sha256 = "1pymw7v2ql42iq825ccx98s4fp9jsz5b2hjr1qad6bamfc6i7yy9";
  };
  phases = [ "buildPhase" ];
  buildPhase = ''
    mkdir -p $out/share/man/man1
    ${pandoc}/bin/pandoc -s -t man $src/thesauron.md -o $out/share/man/man1/thesauron.1
    ${pandoc}/bin/pandoc -s -t man $src/enterprise-patterns.md -o $out/share/man/man1/enterprise-patterns.1
  '';
}
