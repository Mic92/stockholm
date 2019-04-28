{ config, ... }:

with import <stockholm/lib>;

{
  options.mb.hosts = mkOption {
    type = types.attrsOf types.host;
    default =
      filterAttrs (_: host: host.owner.name == "mb" && host.ci)
      config.krebs.hosts;
  };
}
