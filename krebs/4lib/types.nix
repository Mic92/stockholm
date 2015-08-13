{ lib, ... }:

with lib;
with types;

types // rec {

  host = submodule {
    options = {
      name = mkOption {
        type = label;
      };
      dc = mkOption {
        type = label;
      };
      cores = mkOption {
        type = positive;
      };
      nets = mkOption {
        type = attrsOf net;
        apply = x: assert hasAttr "retiolum" x; x;
      };

      secure = mkOption {
        type = bool;
        default = false;
        description = ''
          If true, then the host is capable of keeping secret information.

          TODO define minimum requirements for secure hosts
        '';
      };
    };
  };

  net = submodule ({ config, ... }: {
    options = {
      via = mkOption {
        type = nullOr net;
        default = null;
      };
      addrs = mkOption {
        type = listOf addr;
        apply = _: config.addrs4 ++ config.addrs6;
      };
      addrs4 = mkOption {
        type = listOf addr4;
        default = [];
      };
      addrs6 = mkOption {
        type = listOf addr6;
        default = [];
      };
      aliases = mkOption {
        # TODO nonEmptyListOf hostname
        type = listOf hostname;
      };
      tinc = mkOption {
        type = let net-config = config; in nullOr (submodule ({ config, ... }: {
          options = {
            config = mkOption {
              type = str;
              apply = _: ''
                ${optionalString (net-config.via != null)
                  (concatMapStringsSep "\n" (a: "Address = ${a}") net-config.via.addrs)}
                ${concatMapStringsSep "\n" (a: "Subnet = ${a}") net-config.addrs}
                ${config.pubkey}
              '';
            };
            pubkey = mkOption {
              type = str;
            };
          };
        }));
        default = null;
      };
      zones = mkOption {
        default = [];
        # TODO: string is either MX, NS, A or AAAA
        type = with types; listOf (attrsOf str);
      };
    };
  });

  positive = mkOptionType {
    name = "positive integer";
    check = x: isInt x && x > 0;
    merge = mergeOneOption;
  };

  user = submodule {
    options = {
      mail = mkOption {
        type = str; # TODO retiolum mail address
      };
      name = mkOption {
        type = str; # TODO
      };
      pubkey = mkOption {
        type = str;
      };
      pubkeys = mkOption {
        type = attrsOf str;
        default = {};
      };
    };
  };

  # TODO
  addr = str;
  addr4 = str;
  addr6 = str;
  hostname = str;
  label = str;
}
