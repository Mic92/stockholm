{ lib, ... }:
{
  options.krebs.sitemap = lib.mkOption {
    type = with lib.types; attrsOf sitemap.entry;
    default = {};
  };
}
