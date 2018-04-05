{ stdenv, makeWrapper, lib, buildEnv, fetchgit, nodejs-8_x, pkgs }:

with lib;

let
  nodeEnv = import <nixpkgs/pkgs/development/node-packages/node-env.nix> {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    nodejs = nodejs-8_x;
    libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
  };

  node_env = pkgs.buildEnv {
    name = "go-node_env";
    paths = attrValues (import ./node-packages.nix {
      inherit (pkgs) fetchurl fetchgit;
      inherit nodeEnv;
    });
  };

in stdenv.mkDerivation {
  packageName = "go";
  name = "go-shortener";
  version = "0.0.0";

  src = fetchgit {
    url = "http://cgit.lassul.us/go/";
    rev = "05d02740e0adbb36cc461323647f0c1e7f493156";
    sha256 = "6015c9a93317375ae8099c7ab982df0aa93a59ec2b48972e253887bb6ca0004f";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  buildInputs = [
    nodejs-8_x
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    cp index.js $out/
    cat > $out/go << EOF
      ${nodejs-8_x}/bin/node $out/index.js
    EOF
    chmod +x $out/go

    wrapProgram $out/go \
      --prefix NODE_PATH : ${node_env}/lib/node_modules

    ln -s $out/go /$out/bin/go
  '';

}
