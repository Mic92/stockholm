{ config, lib, ... }: let
  slib = import ../../lib/pure.nix { inherit lib; };

  r6 = ip: (slib.krebs.genipv6 "retiolum" "lass" ip).address;
  w6 = ip: (slib.krebs.genipv6 "wiregrill" "lass" ip).address;
  hostFiles =
    builtins.map (lib.removeSuffix ".nix") (
      builtins.filter
        (x: lib.hasSuffix ".nix" x && x != "default.nix")
        (lib.attrNames (builtins.readDir ./.))
    );

in {
  dns.providers = {
    "lassul.us" = "zones";
  };
  hosts = lib.mapAttrs (_: lib.recursiveUpdate {
    owner = config.krebs.users.lass;
    consul = true;
    ci = false;
    monitoring = true;
  }) (
    lib.genAttrs hostFiles (host: import (./. + "/${host}.nix") {
      inherit config lib r6 w6;
      inherit (slib) krebs;
    })
  );
  users = {
    lass = {
      mail = "lass@green.r";
      pubkey = builtins.readFile ./ssh/yubikey.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/yubikey.pgp;
    };
  };
}
