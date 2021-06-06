{ lib, makeWrapper, stdenv
, cac-api, cac-cert, cac-panel, coreutils, gnumake, gnused, jq, openssh, proot, sshpass
}:

stdenv.mkDerivation rec {
  name = "${shortname}-${version}";
  shortname = "infest-cac-centos7";
  version = "0.2.7";

  src = ./notes;

  phases = [
    "installPhase"
  ];

  buildInputs = [ makeWrapper ];

  path = lib.makeSearchPath "bin" [
    coreutils
    cac-api
    cac-panel
    gnumake
    gnused
    jq
    openssh
    sshpass
    proot
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp ${src} $out/bin/${shortname}
    chmod +x $out/bin/${shortname}
    wrapProgram $out/bin/${shortname} \
        --prefix PATH : ${path} \
        --set REQUESTS_CA_BUNDLE ${cac-cert} \
        --set SSL_CERT_FILE ${cac-cert}
  '';
  meta = with lib; {
    homepage = http://krebsco.de;
    description = "infest a CaC box with stockholm";
    license = licenses.wtfpl;
    maintainers = [ maintainers.makefu ];
  };
}
