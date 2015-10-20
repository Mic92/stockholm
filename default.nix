# Welcome to the top-level default.nix of stockholm.
#
# You can discover the whole thing easily using the `get` utility,
# which can be found at http://cgit.cd.krebsco.de/get/tree/get
# To install `get` on any Nix-enabled system, use:
#
#     nix-env -f /path/to/stockholm -iA pkgs.get
#
# The "current" arguments are used to provide information about the user who's
# evaluating this file.  This information is used to determine which user
# namespace is to be used.  Of course there's nothing trying to prevent you
# from forging this information.  E.g. you could try to generate the deployment
# script for some random user's system, targeting some random host:
#
#     LOGNAME=tv get krebs.deploy system=nomic target=8.8.8.8
#
{ current-date ? abort "current-date not defined"
, current-host-name ? abort "current-host-name not defined"
, current-user-name ? builtins.getEnv "LOGNAME"
}@current:

let out = {
    # The generated scripts to deploy (or infest) systems can be found in the
    # `krebs` attribute.  There's also an init script, but it's in its early
    # stages, not well integrated and mostly useless at the moment. :)
    #
    # You'll also find lib here, which is nixpkgs/lib + krebs lib, but nobody
    # is really accessing this directly, as this lib gets reexported below.
    inherit krebs;

    # All systems of all users can be found here.
    #
    # /!\ Please note that `get users.${user-name}.${host-name}.system` is a
    # bad idea because it will produce vast amounts of output.  These are the
    # actual and complete system derivations that can be installed on the
    # respective host.
    #
    # Another thing to notice here is that other user's systems might not be
    # evaluable because of missing secrets.  If you _are_ able to evaluate
    # another user's system, then you probably share a similar naming scheme
    # for your secret files! :)
    inherit users;

    # Additionally, output lib and pkgs for easy access from the shell.
    # Notice how we're evaluating just the stockholm module to obtain pkgs.
    inherit lib;
    inherit (eval {}) pkgs;
  };

  krebs = import ./krebs (current // { stockholm = out; });
  inherit (krebs) lib;

  # Path resolvers for common and individual files.
  # Example: `upath "3modules"` produces the current user's 3modules directory
  kpath = lib.nspath "krebs";
  upath = lib.nspath current-user-name;

  # This is the base stockholm NixOS module.  It's purpose is to provide a
  # modules and packages, both common ones, found in krebs/ as well as the
  # current user's, found in the user's namespace.
  stockholm = {
    imports = map (f: f "3modules") [ kpath upath ];

    nixpkgs.config.packageOverrides = pkgs:
      let
        # Notice the ordering.  Krebs packages can only depend on Nixpkgs,
        # whereas user packages additionally can depend on krebs packages.
        kpkgs = import (kpath "5pkgs") { inherit pkgs; };
        upkgs = import (upath "5pkgs") { pkgs = pkgs // kpkgs; };
      in
      kpkgs // upkgs;
  };

  # The above stockholm module is used together with a NixOS configuration to
  # produce a system.  Stockholm really just provides additional packages and
  # modules that don't fit upstream for one reason or another.
  # TODO provide krebs lib, so modules don't have to import it awkwardly
  eval = config: import <nixpkgs/nixos/lib/eval-config.nix> {
    modules = [
      stockholm
      config
    ];
  };

  # Any top-level directory other than krebs/ is considered to be a user
  # namespace, configuring a bunch of systems.
  # Have a look at the definition of install in krebs/default.nix to see how
  # nix-env is using this attribute set to obtain the system to be installed.
  # TODO move user namespaces' to users/, so no exception for krebs/ is needed
  users =
    lib.mapAttrs
      (name: _: eval-all-systems (lib.nspath name "1systems"))
      (lib.filterAttrs
        (n: t: !lib.hasPrefix "." n && t == "directory" && n != "krebs")
        (builtins.readDir ./.));

  # Given a path to a user namespace, provide an attribute of evaluated
  # system configurations, keyed by system names (AKA host names).
  eval-all-systems = path:
    lib.mapAttrs'
      (n: _: (lib.nameValuePair (lib.removeSuffix ".nix" n)
                                (eval-system (path + "/${n}"))))
      (builtins.readDir path);

  eval-system = path: rec {
    inherit (eval path) config options;
    system = config.system.build.toplevel;
  };

in out
