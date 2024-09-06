{ config, buildbot-nix, ... }:
{
  imports = [
    buildbot-nix.nixosModules.buildbot-worker
  ];

  services.buildbot-nix.worker = {
    enable = true;
    name = config.krebs.build.host.name;
    workerPasswordFile = "/var/src/secrets/nix-worker-file";
    masterUrl = "tcp:host=gum:port=9989";
  };
}
