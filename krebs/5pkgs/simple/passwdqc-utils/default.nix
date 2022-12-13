{ fetchurl, lib, stdenv
, libxcrypt
, linux-pam
, wordset-file ? null, # set your own wordset-file
}:

stdenv.mkDerivation rec {
  pname = "passwdqc-utils";
  version = "2.0.2";
  buildInputs = [ libxcrypt linux-pam ];

  src = fetchurl {
    url = "http://www.openwall.com/passwdqc/passwdqc-${version}.tar.gz";
    hash = "sha256-/x9QV2TAIPakSEseDMT9vy4/cbUikm2QtHCRBMoGBKs=";
  };

  buildTargets = "utils";
  installFlags= [ "BINDIR=$(out)/bin"
                  "CONFDIR=$(out)/etc"
                  "SHARED_LIBDIR=$(out)/lib"
                  "DEVEL_LIBDIR=$(out)/lib"
                  "SECUREDIR=$(out)/lib/security"
                  "INCLUDEDIR=$(out)/include"
                  "MANDIR=$(out)/man" ];

  patchPhase = lib.optionalString (wordset-file != null) ''
    cp -f ${wordset-file} wordset_4k.c
  '';

  installTargets = "install_lib install_utils";

  meta = {
    description = "passwdqc utils (pwqgen,pwqcheck) and library";
    license = lib.licenses.bsd3;
    maintainers = [ lib.maintainers.makefu ];
    patforms = lib.platforms.linux; # more installFlags must be set for Darwin,Solaris
  };
}
