{ lib, pkgs,stdenv }:
let 
  src = pkgs.fetchFromGitHub {
    owner = "makefu";
    repo = "ukrepl";
    rev = "0baa5cc4d5c3c17af704b69a800dd1f520ded8e3";
    hash = "sha256:1lnhkf02f18fvf3l2fcszvs4x115lql17akabd5ph9ff9z33k8rv";
  };
in
  pkgs.writers.writePython3Bin "ukrepl" {} (builtins.readFile (src + "/ukrepl"))

