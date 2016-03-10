{ stdenv, coreutils, makeWrapper,
  cac-api, cac-cert, cac-panel, gnumake, gnused, jq, openssh, sshpass, proot,
  ... }:

stdenv.mkDerivation rec {
  name = "${shortname}-${version}";
  shortname = "infest-cac-centos7";
  version = "0.2.6";

  src = ./notes;

  phases = [
    "installPhase"
  ];

  buildInputs = [ makeWrapper ];

  path = stdenv.lib.makeSearchPath "bin" [
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
  meta = with stdenv.lib; {
    homepage = http://krebsco.de;
    description = "infest a CaC box with stockholm";
    license = licenses.wtfpl;
    maintainers = [ maintainers.makefu ];
  };
}
