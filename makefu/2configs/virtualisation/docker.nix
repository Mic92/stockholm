{ pkgs, config, ... }:
{
  virtualisation.docker.enable = true;
  environment.systemPackages = with pkgs;[
    docker
    docker_compose
  ];
  users.users.${config.krebs.build.user.name}.extraGroups = [ "docker" ];
}
