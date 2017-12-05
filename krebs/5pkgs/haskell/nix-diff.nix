{ mkDerivation, attoparsec, base, containers, Diff, fetchgit, mtl
, nix-derivation, optparse-generic, stdenv, system-filepath, text
, unix, vector
}:
mkDerivation {
  pname = "nix-diff";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/Gabriel439/nix-diff";
    sha256 = "1k00nx8pannqmpzadkwfrs6bf79yk22ynhd033z5rsyw0m8fcz9k";
    rev = "e32ffa2c7f38b47a71325a042c1d887fb46cdf7d";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base containers Diff mtl nix-derivation optparse-generic
    system-filepath text unix vector
  ];
  homepage = "https://github.com/Gabriel439/nix-diff";
  description = "Explain why two Nix derivations differ";
  license = stdenv.lib.licenses.bsd3;
}
