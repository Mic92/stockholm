{ config, lib, pkgs, ... }:

with import ../../4lib/tv { inherit lib pkgs; };
let
  cfg = config.tv.identity;

  out = {
    options.tv.identity = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "tv.identity";

    self = mkOption {
      type = types.host;
    };

    others = mkOption {
      type = types.host;
      default = filterAttrs (name: _host: name != cfg.self.name) cfg.hosts;
    };

    hosts = mkOption {
      type = with types; attrsOf host;
      apply = mapAttrs (name: value: value // { inherit name; });
    };
  };

  imp = {
    networking.extraHosts =
      concatStringsSep "\n" (flatten (
        # TODO deepMap ["hosts" "nets"] (hostname: host: netname: net:
        mapAttrsToList (hostname: host:
          mapAttrsToList (netname: net:
            let
              aliases = toString (unique (longs ++ shorts));
              longs = (splitByProvider net.aliases).hosts;
              shorts = map (removeSuffix ".${cfg.self.search}") longs;
            in
            map (addr: "${addr} ${aliases}") net.addrs
          ) host.nets
        ) cfg.hosts
      ));
  };

  # TODO move domain name providers to a dedicated module
  # providers : tree label providername
  providers = {
    internet = "hosts";
    retiolum = "hosts";
    de.viljetic = "regfish";
    de.krebsco = "ovh";
  };

  # splitByProvider : [alias] -> set providername [alias]
  splitByProvider = foldl (acc: alias: insert (providerOf alias) alias acc) {};

  # providerOf : alias -> providername
  providerOf = alias:
    tree-get (splitString "." alias) providers;

  # insert : k -> v -> set k [v] -> set k [v]
  insert = name: value: set:
    set // { ${name} = set.${name} or [] ++ [value]; };

  # tree k v = set k (either v (tree k v))

  # tree-get : [k] -> tree k v -> v
  tree-get = path: x:
    let
      y = x.${last path};
    in
    if typeOf y != "set"
      then y
      else tree-get (init path) y;
in
out
