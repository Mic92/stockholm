{ mkDerivation, async, base, base16, base32, bytestring, charset
, fetchgit, http-client, http-types, lib, managed, megaparsec, mtl
, network, nix, optparse-applicative, tasty-bench, temporary, text
, turtle, vector, wai, wai-extra, warp, warp-tls
, boost
}:
mkDerivation {
  pname = "nix-serve-ng";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/aristanetworks/nix-serve-ng";
    sha256 = "0mqp67z5mi8rsjahdh395n7ppf0b65k8rd3pvnl281g02rbr69y2";
    rev = "433f70f4daae156b84853f5aaa11987aa5ce7277";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base16 base32 bytestring charset http-types managed megaparsec
    mtl network optparse-applicative vector wai wai-extra warp warp-tls
  ];
  executablePkgconfigDepends = [ nix ];
  executableSystemDepends = [ boost.dev ];
  benchmarkHaskellDepends = [
    async base bytestring http-client tasty-bench temporary text turtle
    vector
  ];
  description = "A drop-in replacement for nix-serve that's faster and more stable";
  license = lib.licenses.bsd3;
}
