{ config, lib, pkgs, ... }:

with lib;
{
  krebs.repo-sync = let
    # TODO addMirrorURL function
    mirror = "git@wolf:stockholm-mirror";
  in {
    enable = true;
    config = {
      makefu = {
        origin.url = http://cgit.gum/stockholm ;
        mirror.url = mirror;
      };
      tv = {
        origin.url = http://cgit.cd/stockholm ;
        mirror.url = mirror;
      };
      lassulus = {
        origin.url = http://cgit.cloudkrebs/stockholm ;
        mirror.url = mirror;
      };
      "@latest" = {
        mirror.url = mirror;
      };
    };
  };
}
