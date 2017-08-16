{ config, ... }:
let
  url = "https://git.shackspace.de/";
  # generate token from CI-token via:
  ## gitlab-runner register
  ## cat /etc/gitlab-runner/config.toml
  token = import <secrets/shackspace-gitlab-ci-token.nix> ;
in {
  systemd.services.gitlab-runner.path = [ 
    "/run/wrappers" # /run/wrappers/bin/su
    "/" # /bin/sh
  ];
  virtualisation.docker.enable = true;
  services.gitlab-runner = {
    enable = true;
    # configFile, configOptions and gracefulTimeout not yet in stable
    # gracefulTimeout = "120min";
    configText = ''
    concurrent = 1
    check_interval = 0

    [[runners]]
      name = "krebs-shell"
      url = "${url}"
      token = "${token}"
      executor = "shell"
      shell = "sh"
      environment = ["PATH=/bin:/run/wrappers/bin:/etc/per-user/gitlab-runner/bin:/etc/per-user-pkgs/gitlab-runner/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin"]
      [runners.cache]

    '';
  };
}
