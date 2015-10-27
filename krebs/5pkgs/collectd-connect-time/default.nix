{lib, pkgs, pythonPackages, fetchurl, ... }:

pythonPackages.buildPythonPackage rec {
  name = "collectd-connect-time-${version}";
  version = "0.2.1";
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/c/collectd-connect-time/collectd-connect-time-${version}.tar.gz";
    sha256 = "0611h53ww9lk1qm27njsffckkibirmq7p8cxlq02dgl1zbh7583d";
  };
  meta = {
    homepage = https://pypi.python.org/pypi/collectd-connect-time/;
    description = "TCP Connection time plugin for collectd";
    license = lib.licenses.wtfpl;
  };
}
