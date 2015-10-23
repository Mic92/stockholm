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
    rev = "b22729670236bfa6491207d57c5d7565137625ca";
    sha256 = "8ff00de56d85543399776c82d41d92ccc68000e5dce0f008d926748e188f3c69";
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
