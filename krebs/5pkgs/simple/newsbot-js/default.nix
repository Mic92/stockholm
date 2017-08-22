{ stdenv, makeWrapper, callPackage, lib, buildEnv, fetchgit, nodePackages, nodejs }:

with lib;

let
  np = (callPackage <nixpkgs/pkgs/top-level/node-packages.nix>) {
    generated = ./packages.nix;
    self = np;
  };

  node_env = buildEnv {
    name = "node_env";
    paths = [
      np.feedparser
      np.form-data
      np.irc
      np.request
      np.shell-quote
    ];
    pathsToLink = [ "/lib" ];
    ignoreCollisions = true;
  };

in np.buildNodePackage {
  name = "newsbot-js";

  src = fetchgit {
    url = "http://cgit.prism/newsbot-js/";
    rev = "09e01639be4ea9691cf5b33f7d9057b68ac98079";
    sha256 = "28ffbed66c2efcd194c47823c7d5d5533c80852fc0cf9d9d4ee609c71d50c142";
  };

  phases = [
    "unpackPhase"
    "patchPhase"
    "installPhase"
  ];

  deps = (filter (v: nixType v == "derivation") (attrValues np));

  buildInputs = [
    nodejs
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    cp newsbot.js $out/
    cat > $out/newsbot << EOF
      ${nodejs}/bin/node $out/newsbot.js
    EOF
    chmod +x $out/newsbot

    wrapProgram $out/newsbot \
      --prefix NODE_PATH : ${node_env}/lib/node_modules

     ln -s $out/newsbot /$out/bin/newsbot
  '';

}
