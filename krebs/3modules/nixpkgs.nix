{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.krebs.nixpkgs;

  out = {
    options.krebs.nixpkgs = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.nixpkgs" // { default = true; };

    allowUnfreePredicate = mkOption {
      description = ''
        This option is similar to `nixpkgs.config.allowUnfreePredicate'
        but can be defined in several modules.  An unfree package will be
        allowed if any of the defined predicates returns true.
      '';
      type = types.nullOr (mkOptionType {
        name = "Predicate";
        check = isFunction;
        merge = _locs: defs: pkg: let
          evalPredicateDef = def: let
            allow = def.value pkg;
          in if cfg.verbose && allow
            then trace "unfree ‘${pkg.name}’ allowed in ${def.file}" allow
            else allow;
        in any evalPredicateDef defs;
      });
      default = null;
    };

    verbose = mkOption {
      type = types.bool;
      default = false;
    };
  };

  imp = lib.mkIf (cfg.allowUnfreePredicate != null) {
    nixpkgs.config.allowUnfreePredicate = cfg.allowUnfreePredicate;
  };
in out
