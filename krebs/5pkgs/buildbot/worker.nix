{ pkgs, stdenv, fetchurl, pythonPackages }:
pythonPackages.buildPythonApplication (rec {
  name = "${pname}-${version}";
  pname = "buildbot-worker";
  version = "0.9.4";

  doCheck = false;
  src = fetchurl {
    url = "mirror://pypi/b/${pname}/${name}.tar.gz";
    sha256 = "0rdrr8x7sn2nxl51p6h9ad42s3c28lb6sys84zrg0d7fm4zhv7hj";
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

