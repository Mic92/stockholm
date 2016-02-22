{ stdenv, fetchgit, bc, cac-cert, coreutils, curl, dash, gnugrep, gnused, inotifyTools, jq, ncurses, openssh, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-api-1.1.1";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/cac-api;
    rev = "46c7af2935ccc096ba0e93cd1adf575026edf44a";
    sha256 = "0i8aspkmfw74np7hlbip3hk7zbgl6cxrnbg83x4wgqrj5dpx6vy0";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin
    { cat <<\EOF
    #! ${dash}/bin/dash
    export PATH=${stdenv.lib.makeSearchPath "bin" [
      bc
      coreutils
      curl
      gnugrep
      gnused
      inotifyTools
      jq
      ncurses
      openssh
      sshpass
    ]}:"$PATH"
    EOF
      # [1]: Disable fetching tasks; listtasks is currently broken:
      # Unknown column 'iod.apitask.cid' in 'field list'
      sed '
        /^\s*tasks \\$/d; # [1]
        s|\<_cac_exec curl|<${cac-cert} & --cacert /dev/stdin|
      ' cac-api
    } > $out/bin/cac-api
    chmod +x $out/bin/cac-api
  '';
}
