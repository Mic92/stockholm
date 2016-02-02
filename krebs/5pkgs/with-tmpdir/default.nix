{ stdenv, fetchgit, coreutils, dash, ... }:

stdenv.mkDerivation {
  name = "with-tmpdir-1";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/with-tmpdir;
    rev = "3243c02ed8cd27a04c080bd39560204980f6c16a";
    sha256 = "80ee6cafb2c337999ddcd1e41747d6256b7cfcea605358c2046eb7e3729555c6";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin

    { echo '#! ${dash}/bin/dash'
      echo 'OLDPATH=$PATH'
      echo 'PATH=${coreutils}/bin'
      sed '$s/^/#/' ./with-tmpdir
      echo '(PATH=$OLDPATH; exec "$@")'
    } > $out/bin/with-tmpdir

    chmod +x $out/bin/with-tmpdir
  '';
}
