{ config, ... }:

with import <stockholm/lib>;

{
  options.tv.hosts = mkOption {
    type = types.attrsOf types.host;
    default =
      filterAttrs (_: host: host.owner.name == "tv")
      config.krebs.hosts;
  };
}
