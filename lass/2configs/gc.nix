{ config, ... }:

with import <stockholm/lib>;
{
  nix.gc = {
    automatic = ! (elem config.krebs.build.host.name [ "aergia" "mors" "xerxes" "coaxmetal" ] || config.boot.isContainer);
    options = "--delete-older-than 15d";
  };
}
