{ stdenv, coreutils,makeWrapper, cac, cacpanel, gnumake, gnused, jq, openssh, ... }:

stdenv.mkDerivation rec {
  name = "krebs-ci-0.1.0";

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
      cp ${src} $out/bin/krebs-ci
      chmod +x $out/bin/krebs-ci
      wrapProgram $out/bin/krebs-ci \
              --prefix PATH : ${path}
    '';
  meta = with stdenv.lib; {
    homepage = http://krebsco.de;
    description = "Krebs CI Scripts";
    license = licenses.wtfpl;
    maintainers = [ maintainers.makefu ];
  };
}
