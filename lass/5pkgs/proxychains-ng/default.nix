{ lib
, stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation rec {
  pname = "proxychains-ng";
  version = "4.15";

  src = fetchFromGitHub {
    owner = "rofl0r";
    repo = pname;
    rev = "v${version}";
    sha256 = "128d502y8pn7q2ls6glx9bvibwzfh321sah5r5li6b6iywh2zqlc";
  };
}
