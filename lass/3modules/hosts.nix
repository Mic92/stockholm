{ config, ... }:

with import <stockholm/lib>;

{
  options.lass.hosts = mkOption {
    type = types.attrsOf types.host;
    default =
      filterAttrs (_: host: host.owner.name == "lass")
      config.krebs.hosts;
  };
}
