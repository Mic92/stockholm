# Same as upstream but with doCheck = false because doctest has wrong version.
{ mkDerivation, base, bytestring, containers, directory
, filepath, unordered-containers, zlib, stdenv
}:
mkDerivation {
  pname = "hyphenation";
  version = "0.6";
  sha256 = "2f673666c18f63581422f7c6389b78b0ff754406671296a3d680d417942512f7";
  libraryHaskellDepends = [
    base bytestring containers unordered-containers zlib
  ];
  homepage = "http://github.com/ekmett/hyphenation";
  description = "Configurable Knuth-Liang hyphenation";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
  doCheck = false;
}
