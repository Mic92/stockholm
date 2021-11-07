{ mkDerivation, base, lib, template-haskell, text }:
mkDerivation {
  pname = "th-env";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base template-haskell text ];
  homepage = "https://stackoverflow.com/q/57635686";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
