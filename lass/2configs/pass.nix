{ config, pkgs, ... }:

{
  krebs.per-user.lass.packages = with pkgs; [
    pass
    gnupg1
  ];

}
