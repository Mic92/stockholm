{ stdenv, makeWrapper, lib, buildEnv, fetchgit, nodePackages, nodejs }:

with lib;

let
  np = nodePackages.override {
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

in nodePackages.buildNodePackage {
  name = "newsbot-js";

  src = fetchgit {
    url = "http://cgit.echelon/newsbot-js/";
    rev = "802b172d0eed6c9625a9cb5db408f5cc8c01784e";
    sha256 = "794fc7845aca311f7cf7b6bdc109b5a25d0e2299322bc6612edadc477b2536e2";
  };

  phases = [
    "unpackPhase"
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
