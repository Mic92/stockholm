{ config, lib, pkgs, ... }:

with lib;

{
  krebs.per-user.xr.packages = [
    pkgs.cr
  ];

  security.sudo.extraConfig = "tv ALL=(xr) NOPASSWD: ALL";

  users.users.xr = {
    extraGroups = [
      "audio"
      "video"
    ];
    group = "subusers";
    home = "/home/xr";
    uid = 1660006127; # genid xr
    useDefaultShell = true;
  };
}
