{ config, lib, pkgs, ... }:
{
  services.nginx.package = lib.mkDefault (pkgs.nginxStable.override { openssl = pkgs.libressl; });
}
