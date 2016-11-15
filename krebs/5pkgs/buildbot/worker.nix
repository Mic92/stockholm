{ pkgs, stdenv, fetchurl, pythonPackages }:
pythonPackages.buildPythonApplication (rec {
  name = "${pname}-${version}";
  pname = "buildbot-worker";
  version = "0.9.1";

  doCheck = false;
  src = fetchurl {
    url = "mirror://pypi/b/${pname}/${name}.tar.gz";
    sha256 = "00p9l1qz6mx12npjwsycp8f9a8f2har15ig79pfsg8z7a7yw93hx";
  };

  buildInputs = with pythonPackages; [ setuptoolsTrial mock ];
  propagatedBuildInputs = with pythonPackages; [ twisted future pkgs.treq ];

  meta = with stdenv.lib; {
    homepage = http://buildbot.net/;
    description = "Buildbot Worker Daemon";
    maintainers = with maintainers; [ nand0p ryansydnor ];
    platforms = platforms.all;
    license = licenses.gpl2;
  };
})

