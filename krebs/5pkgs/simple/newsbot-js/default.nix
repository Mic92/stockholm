{ stdenv, makeWrapper, lib, buildEnv, fetchgit, nodejs-12_x, pkgs, icu }:

with lib;

let
  nodeEnv = import <nixpkgs/pkgs/development/node-packages/node-env.nix> {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    nodejs = nodejs-12_x;
    libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
  };

  node_env = pkgs.buildEnv {
    name = "go-node_env";
    paths = attrValues (import ./node-packages.nix {
      inherit (pkgs) fetchurl fetchgit;
      inherit nodeEnv;
      globalBuildInputs = [
        icu.dev
      ];
    });
  };


in stdenv.mkDerivation {
  name = "newsbot-js";

  src = fetchgit {
    url = "http://cgit.prism/newsbot-js/";
    rev = "09e01639be4ea9691cf5b33f7d9057b68ac98079";
    sha256 = "28ffbed66c2efcd194c47823c7d5d5533c80852fc0cf9d9d4ee609c71d50c142";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  buildInputs = [
    nodejs-12_x
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    cp newsbot.js $out/
    cat > $out/newsbot << EOF
      ${nodejs-12_x}/bin/node $out/newsbot.js
    EOF
    chmod +x $out/newsbot

    wrapProgram $out/newsbot \
      --prefix NODE_PATH : ${node_env}/lib/node_modules

     ln -s $out/newsbot /$out/bin/newsbot
  '';

}
