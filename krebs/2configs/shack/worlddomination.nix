{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  pkg = pkgs.stdenv.mkDerivation {
    name = "worlddomination-2018-04-21";
    src = pkgs.fetchgit {
      url = "https://github.com/shackspace/worlddomination/";
      rev = "1b32403b9";
      sha256 = "10x7aiil13k3x9wqy95mi1ys999d6fxg5sys3jwv7a1p930gkl1i";
    };
    buildInputs = [
      (pkgs.python3.withPackages (pythonPackages: with pythonPackages; [
        docopt
        LinkHeader
        aiocoap
        grequests
        paramiko
        python
      ]))
    ];
    installPhase = ''
      install -m755 -D backend/push_led.py  $out/bin/push-led
      install -m755 -D backend/loop_single.py  $out/bin/loop-single
      # copy the provided file to the package
      install -m755 -D backend/wd.lst  $out/${wdpath}
    '';
  };
  pythonPackages = pkgs.python3Packages;
  # https://github.com/chrysn/aiocoap
  grequests = pythonPackages.buildPythonPackage rec {
    pname = "grequests";
    version = "0.3.1";
    name = "${pname}-${version}";

    src = pkgs.fetchFromGitHub {
      owner = "kennethreitz";
      repo = "grequests";
      rev =  "d1e70eb";
      sha256 = "0drfx4fx65k0g5sj0pw8z3q1s0sp7idn2yz8xfb45nd6v82i37hc";
    };

    doCheck = false;

    propagatedBuildInputs = with pythonPackages; [ requests gevent ];

    meta = with lib;{
      description = "Asynchronous HTTP requests";
      homepage = https://github.com/kennethreitz/grequests;
      license = with licenses; [ bsd2 ];
      maintainers = with maintainers; [ matejc ];
    };
  };

  aiocoap = pythonPackages.buildPythonPackage {
      name = "aiocoap-0.3";
      src = pkgs.fetchurl { url = "https://pypi.python.org/packages/9c/f6/d839e4b14258d76e74a39810829c13f8dd31de2bfe0915579b2a609d1bbe/aiocoap-0.3.tar.gz"; sha256 = "402d4151db6d8d0b1d66af5b6e10e0de1521decbf12140637e5b8d2aa9c5aef6"; };
      propagatedBuildInputs = [ ];
      doCheck = false; # 2 errors, dunnolol
      meta = with pkgs.stdenv.lib; {
        homepage = "";
        license = licenses.mit;
        description = "Python CoAP library";
      };
    };
  LinkHeader = pythonPackages.buildPythonPackage {
    name = "LinkHeader-0.4.3";
    src = pkgs.fetchurl { url = "https://pypi.python.org/packages/27/d4/eb1da743b2dc825e936ef1d9e04356b5701e3a9ea022c7aaffdf4f6b0594/LinkHeader-0.4.3.tar.gz"; sha256 = "7fbbc35c0ba3fbbc530571db7e1c886e7db3d718b29b345848ac9686f21b50c3"; };
    propagatedBuildInputs = [ ];
    meta = with pkgs.stdenv.lib; {
      homepage = "";
      license = licenses.bsdOriginal;
      description = "Parse and format link headers according to RFC 5988 \"Web Linking\"";
    };
  };
  wdpath = "/usr/worlddomination/wd.lst";
  esphost = "10.42.24.7"; # esp8266
  timeout = 10; # minutes
in {
  systemd.services.worlddomination = {
    description = "run worlddomination";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "nobody"; # TODO separate user
      ExecStart = "${pkg}/bin/push-led ${esphost} ${pkg}/${wdpath} loop ${toString timeout}";
      Restart = "always";
      PrivateTmp = true;
      PermissionsStartOnly = true;
    };
  };
}
