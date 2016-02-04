{ stdenv, fetchgit, bc, coreutils, curl, dash, gnused, inotifyTools, jq, ncurses, openssh, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-api-1.1.0";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/cac-api;
    rev = "0809fae379239687ed1170e04311dc2880ef0aba";
    sha256 = "357ced27c9ed88028967c934178a1d230bf38617a7494cd4632fabdd2a04fcdd";
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
      gnused
      inotifyTools
      jq
      ncurses
      openssh
      sshpass
    ]}
    EOF
      # [1]: Disable fetching tasks; listtasks is currently broken:
      # Unknown column 'iod.apitask.cid' in 'field list'
      sed '
        /^\s*tasks \\$/d; # [1]
        s|\<_cac_exec curl|<${./cac.pem} & --cacert /dev/stdin|
      ' cac-api
    } > $out/bin/cac-api
    chmod +x $out/bin/cac-api
  '';
}
