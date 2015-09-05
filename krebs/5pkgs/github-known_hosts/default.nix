{ lib, ... }:

with builtins;
with lib;

let
  github-pubkey = removeSuffix "\n" (readFile ./github.ssh.pub);
in

toFile "github-known_hosts"
  (concatMapStrings
    (i: "github.com,192.30.252.${toString i} ${github-pubkey}\n")
    (range 0 255))
