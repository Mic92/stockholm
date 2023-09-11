{ mkDerivation, async, base, base16, base32, bytestring, charset
, fetchgit, http-client, http-types, lib, managed, megaparsec, mtl
, network, nix, optparse-applicative, tasty-bench, temporary, text
, turtle, vector, wai, wai-extra, warp, warp-tls
, boost
}:
mkDerivation {
  pname = "nix-serve-ng";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/aristanetworks/nix-serve-ng";
    sha256 = "sha256-PkzwtjUgYuqfWtCH1nRqVRaajihN1SqMVjWmoSG/CCY=";
    rev = "9b546864f4090736f3f9069a01ea5d42cf7bab7c";
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
