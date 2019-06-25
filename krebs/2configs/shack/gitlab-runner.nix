{ pkgs, ... }:
let
  runner-src = builtins.fetchTarball {
    url = "https://gitlab.com/arianvp/nixos-gitlab-runner/-/archive/master/nixos-gitlab-runner-master.tar.gz";
    sha256 = "1s0fy5ny2ygcfvx35xws8xz5ih4z4kdfqlq3r6byxpylw7r52fyi";
  };
in
{
  systemd.services.gitlab-runner.path = [
    "/run/wrappers" # /run/wrappers/bin/su
    "/" # /bin/sh
  ];
  imports = [
    "${runner-src}/gitlab-runner.nix"
  ];
  services.gitlab-runner2.enable = true;
  ## registrationConfigurationFile contains:
  # CI_SERVER_URL=<CI server URL>
  # REGISTRATION_TOKEN=<registration secret>
  services.gitlab-runner2.registrationConfigFile = <secrets/shackspace-gitlab-ci>;
}
