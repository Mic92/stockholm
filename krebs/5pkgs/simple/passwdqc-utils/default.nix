{ fetchurl, lib, stdenv
, libxcrypt
, pam
, wordset-file ? null, # set your own wordset-file
}:

stdenv.mkDerivation rec {
  name = "passwdqc-utils-${version}";
  version = "1.3.0";
  buildInputs = [ libxcrypt pam ];

  src = fetchurl {
    url = "http://www.openwall.com/passwdqc/passwdqc-${version}.tar.gz";
    sha256 = "0l3zbrp4pvah0dz33m48aqlz9nx663cc1fqhnlwr0p853b10la93";
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
