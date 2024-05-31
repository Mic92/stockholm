{ buildbot-nix, ... }:
{
  imports = [
    buildbot-nix.nixosModules.buildbot-worker
  ];

  services.buildbot-nix.worker = {
    enable = true;
    workerPasswordFile = "/var/src/secrets/nix-worker-file";
  };
}
