{lib, pkgs, pythonPackages, fetchurl, ... }:

pythonPackages.buildPythonPackage rec {
  name = "collectd-connect-time-${version}";
  version = "0.3.0";
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/c/collectd-connect-time/collectd-connect-time-${version}.tar.gz";
    sha256 = "0vvrf9py9bwc8hk3scxwg4x2j8jlp2qva0mv4q8d9m4b4mk99c95";
  };
  meta = {
    homepage = https://pypi.python.org/pypi/collectd-connect-time/;
    description = "TCP Connection time plugin for collectd";
    license = lib.licenses.wtfpl;
  };
}
