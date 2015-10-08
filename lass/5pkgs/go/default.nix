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
      np.redis
      np."formidable"
    ];
    pathsToLink = [ "/lib" ];
    ignoreCollisions = true;
  };

in nodePackages.buildNodePackage {
  name = "go";

  src = fetchgit {
    url = "http://cgit.echelon/go/";
    rev = "05d02740e0adbb36cc461323647f0c1e7f493156";
    sha256 = "6015c9a93317375ae8099c7ab982df0aa93a59ec2b48972e253887bb6ca0004f";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  deps = (filter (v: nixType v == "derivation") (attrValues np));

  buildInputs = [
    nodejs
    nodePackages.redis
    np.formidable
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    cp index.js $out/
    cat > $out/go << EOF
      ${nodejs}/bin/node $out/index.js
    EOF
    chmod +x $out/go

    wrapProgram $out/go \
      --prefix NODE_PATH : ${node_env}/lib/node_modules

     ln -s $out/go /$out/bin/go
  '';

}
