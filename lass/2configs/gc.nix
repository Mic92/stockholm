{ config, ... }:

with import <stockholm/lib>;
{
  nix.gc = {
    automatic = ! elem config.krebs.build.host.name [ "prism" "mors" ];
  };
}
