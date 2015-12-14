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
    rev = "6ee488430c6915eeae03f1569084577d39cef51d";
    sha256 = "00xmn7hzcs0mm6hjf5i37d9nna5rcd0gra0ynch7x2id8liazksx";
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
