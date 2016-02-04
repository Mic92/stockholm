{ stdenv, coreutils,makeWrapper, cac-api, cacpanel, gnumake, gnused, jq, openssh, ... }:

stdenv.mkDerivation rec {
  name = "${shortname}-${version}";
  shortname = "infest-cac-centos7";
  version = "0.2.0";

  src = ./notes;

  phases = [
    "installPhase"
  ];
  buildInputs = [ makeWrapper ];

  path = stdenv.lib.makeSearchPath "bin" [
    coreutils
    cac-api
    cacpanel
    gnumake
    gnused
    jq
    openssh
  ];

  installPhase =
    ''
      mkdir -p $out/bin
      cp ${src} $out/bin/${shortname}
      chmod +x $out/bin/${shortname}
      wrapProgram $out/bin/${shortname} \
              --prefix PATH : ${path} \
              --set SSL_CERT_FILE ${./panel.cloudatcost.com.crt} \
              --set REQUESTS_CA_BUNDLE ${./panel.cloudatcost.com.crt}
    '';
  meta = with stdenv.lib; {
    homepage = http://krebsco.de;
    description = "Krebs CI Scripts";
    license = licenses.wtfpl;
    maintainers = [ maintainers.makefu ];
  };
}
