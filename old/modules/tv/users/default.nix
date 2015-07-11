{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.tv.users;

  opts = {
    enable = mkOption {
      default = true;
      type = types.bool;
      description = ''
        If set to false, TODO...
      '';
    };

    packages = mkOption {
      default = [];
      #example = literalExample "[ pkgs.firefox pkgs.thunderbird ]";
      type = with types; listOf path;
      description = ''
        TODO this description is for environment.systemPackages
        The set of packages that appear in
        /run/current-system/sw.  These packages are
        automatically available to all users, and are
        automatically updated every time you rebuild the system
        configuration.  (The latter is the main difference with
        installing them in the default profile,
        <filename>/nix/var/nix/profiles/default</filename>.
      '';
    };
  };
in

{
  options.tv.users = mkOption {
    default = {};
    type = with types; attrsOf optionSet;
    options = [ opts ];
    description = ''
      TODO
    '';
  };

  config = {
    system.activationScripts."tv.users" =
      let
        bindir = name: packages:
          pkgs.symlinkJoin "${name}-bindir" (map (path: path + "/" + "bin") packages);
      in
      ''
      mkdir -m 0755 -p /run/tv.users
      # TODO delete old
      # TODO detect collisions
      # TODO don't link .xxx-wrapped
      ${concatStrings (mapAttrsToList (name: { packages, ... }: ''
        mkdir -m 0755 -p /run/tv.users/${name}
        ln -snf ${bindir name packages} /run/tv.users/${name}/bin
      '') cfg)}
    '';
    environment.shellInit = ''
      # XXX lower precedence than ~/bin
      PATH=/run/tv.users/$LOGNAME/bin:$PATH
      export PATH
    '';
  };
}
