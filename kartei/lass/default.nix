with import ../../lib;
{ config, ... }: let

  r6 = ip: (krebs.genipv6 "retiolum" "lass" ip).address;
  w6 = ip: (krebs.genipv6 "wiregrill" "lass" ip).address;
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
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.lass;
    consul = true;
    ci = true;
    monitoring = true;
  }) (
    lib.genAttrs hostFiles (host: import (./. + "/${host}.nix") { inherit config krebs lib r6 w6; })
  );
  users = rec {
    lass = lass-yubikey;
    lass-yubikey = {
      mail = "lass@lassul.us";
      pubkey = builtins.readFile ./ssh/yubikey.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/yubikey.pgp;
    };
    lass-blue = {
      mail = "lass@blue.r";
      pubkey = builtins.readFile ./ssh/blue.rsa;
    };
    lass-green = {
      mail = "lass@green.r";
      pubkey = builtins.readFile ./ssh/green.ed25519;
    };
    lass-red = {
      mail = "lass@red.r";
      pubkey = builtins.readFile ./ssh/red.ed25519;
    };
    lass-mors = {
      mail = "lass@mors.r";
      pubkey = builtins.readFile ./ssh/mors.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/mors.pgp;
    };
    lass-android = {
      mail = "lassulus@gmail.com";
      pubkey = builtins.readFile ./ssh/android.ed25519;
    };
    lass-tablet = {
      pubkey = builtins.readFile ./ssh/tablet.ed25519;
    };
  };
}
