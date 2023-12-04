{ r6, w6, ... }:
{
  ci = false;
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.25";
      ip6.addr = r6 "16a2";
      aliases = [
        "ignavia.r"
      ];
      tinc = {
        pubkey = builtins.readFile ./ignavia/retiolum.rsa_key.pub;
        pubkey_ed25519 = builtins.replaceStrings [ "Ed25519PublicKey = " ] [ "" ] (builtins.readFile ./ignavia/retiolum.ed25519_key.pub);
      };
    };
  };
  ssh.pubkey = builtins.readFile ./ignavia/ssh.id_ed25519.pub;
  syncthing.id = builtins.replaceStrings [ "\n" ] [ "" ] (builtins.readFile ./ignavia/syncthing.pub);
}
