{lib, pkgs, pythonPackages, fetchurl, ... }:

pythonPackages.buildPythonPackage rec {
  name = "collectd-connect-time-${version}";
  version = "0.1.1";
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/c/collectd-connect-time/collectd-connect-time-${version}.tar.gz";
    sha256 = "0j2glcm4qjry1hqgci84ifj83xjpannl5yckyjadc8s9vyf0mp99";
  };
  meta = {
    homepage = https://pypi.python.org/pypi/collectd-connect-time/;
    description = "TCP Connection time plugin for collectd";
    license = lib.licenses.wtfpl;
  };
}
