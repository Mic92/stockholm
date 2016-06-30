{ config, ... }:

with config.krebs.lib;
{
  nix.gc = {
    automatic = ! elem config.krebs.build.host.name [ "prism" "mors" ];
  };
}
