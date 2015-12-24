{ stdenv, coreutils,makeWrapper, cac, cacpanel, gnumake, gnused, jq, openssh, ... }:

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
    cac
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
              --prefix PATH : ${path}
    '';
  meta = with stdenv.lib; {
    homepage = http://krebsco.de;
    description = "Krebs CI Scripts";
    license = licenses.wtfpl;
    maintainers = [ maintainers.makefu ];
  };
}
