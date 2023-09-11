{ config, lib, ... }:
with lib;
{
  users.extraUsers =
    mapAttrs (_: h: { hashedPassword = h; })
             (import "${config.krebs.secret.directory}/hashedPasswords.nix");
}
