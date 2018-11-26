{ fetchgit }:

let
  src = fetchgit {
    url = https://cgit.krebsco.de/disko;
    rev = "16cd458af06d3caf687eb7d80ca3df26b71fe28c";
    sha256 = "16cd458af06d3caf687eb7d80ca3df26b71fe28c";
  };
in

{
  lib = import "${src}/lib";
}
