{lib, pkgs, pythonPackages, fetchurl, ... }:

pythonPackages.buildPythonPackage rec {
  name = "collectd-connect-time-${version}";
  version = "0.2.0";
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/c/collectd-connect-time/collectd-connect-time-${version}.tar.gz";
    sha256 = "1l63pi79z6vg4xf744a71g5rxgsngpkc96a7y4ha2x798r55g7v6";
  };
  meta = {
    homepage = https://pypi.python.org/pypi/collectd-connect-time/;
    description = "TCP Connection time plugin for collectd";
    license = lib.licenses.wtfpl;
  };
}
