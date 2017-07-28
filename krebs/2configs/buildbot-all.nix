{ lib, config, pkgs, ... }:
{
  imports = [
    <stockholm/krebs/2configs/buildbot-krebs.nix>
  ];
  krebs.ci.users.lass.all = true;
  krebs.ci.users.makefu.all = true;
  krebs.ci.users.nin.all = true;
  krebs.ci.users.tv.all = true;
}

