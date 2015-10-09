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
    rev = "cd32ef7b39819f53c7125b22c594202724cc8754";
    sha256 = "425e800f7638a5679ed8a049614a7533f3c8dd09659061885240dc93952ff0ae";
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
