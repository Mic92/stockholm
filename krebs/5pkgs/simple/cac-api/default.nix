{ fetchgit, lib, stdenv
, bc, cac-cert, coreutils, curl, dash, gnugrep, gnused, inotify-tools, jq, ncurses, openssh, sshpass
}:

stdenv.mkDerivation {
  name = "cac-api-1.1.2";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/cac-api;
    rev = "67e93510e7742acae44db30275abbfe671aa9b7b";
    sha256 = "1vxh57j7vrq5sg9j1sam0538kkkhqpgf230vvdz2ifzgkj01z27l";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin
    { cat <<\EOF
    #! ${dash}/bin/dash
    export PATH=${lib.makeBinPath [
      bc
      coreutils
      curl
      gnugrep
      gnused
      inotify-tools
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
