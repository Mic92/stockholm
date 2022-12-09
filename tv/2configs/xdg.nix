with import ./lib;
{ config, pkgs, ... }: {
  environment.variables.XDG_RUNTIME_DIR = "/run/xdg/$LOGNAME";

  systemd.tmpfiles.rules = let
    forUsers = flip map users;
    isUser = { name, group, ... }:
      name == "root" || hasSuffix "users" group;
    users = filter isUser (mapAttrsToList (_: id) config.users.users);
  in forUsers (u: "d /run/xdg/${u.name} 0700 ${u.name} ${u.group} -");
}
