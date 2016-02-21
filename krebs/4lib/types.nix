{ lib, ... }:

with builtins;
with lib;
with types;

types // rec {

  host = submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = label;
        default = config._module.args.name;
      };
      cores = mkOption {
        type = positive;
      };
      nets = mkOption {
        type = attrsOf net;
        default = {};
      };

      owner = mkOption {
        type = user;
        # TODO proper user
        default = {
          name = "krebs";
          mail = "spam@krebsco.de";
        };
      };

      extraZones = mkOption {
        default = {};
        # TODO: string is either MX, NS, A or AAAA
        type = with types; attrsOf string;
      };

      secure = mkOption {
        type = bool;
        default = false;
        description = ''
          If true, then the host is capable of keeping secret information.

          TODO define minimum requirements for secure hosts
        '';
      };

      ssh.pubkey = mkOption {
        type = nullOr str;
        default = null;
        apply = x:
          if x != null
            then x
            else trace "The option `krebs.hosts.${config.name}.ssh.pubkey' is unused." null;
      };
      ssh.privkey = mkOption {
        type = nullOr (submodule {
          options = {
            bits = mkOption {
              type = nullOr (enum ["4096"]);
              default = null;
            };
            path = mkOption {
              type = either path str;
              apply = x: {
                path = toString x;
                string = x;
              }.${typeOf x};
            };
            type = mkOption {
              type = enum ["rsa" "ed25519"];
              default = "ed25519";
            };
          };
        });
        default = null;
      };
    };
  });

  net = submodule ({ config, ... }: {
    options = {
      via = mkOption {
        type = nullOr net;
        default = null;
      };
      addrs = mkOption {
        type = listOf addr;
        default = config.addrs4 ++ config.addrs6;
        # TODO only default addrs make sense
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
        default = [];
      };
      ssh = mkOption {
        type = submodule {
          options = {
            port = mkOption {
              type = nullOr int;
              default = null;
            };
          };
        };
        default = {};
      };
      tinc = mkOption {
        type = let net = config; in nullOr (submodule ({ config, ... }: {
          options = {
            config = mkOption {
              type = str;
              default = concatStringsSep "\n" (
                (optionals (net.via != null)
                  (map (a: "Address = ${a}") net.via.addrs))
                ++
                (map (a: "Subnet = ${a}") net.addrs)
                ++
                [config.pubkey]
              );
            };
            pubkey = mkOption {
              type = str;
            };
          };
        }));
        default = null;
      };
    };
  });

  positive = mkOptionType {
    name = "positive integer";
    check = x: isInt x && x > 0;
    merge = mergeOneOption;
  };

  secret-file = submodule ({ config, ... }: {
    options = {
      path = mkOption { type = str; };
      mode = mkOption { type = str; default = "0400"; };
      owner-name = mkOption { type = str; default = "root"; };
      group-name = mkOption { type = str; default = "root"; };
      source-path = mkOption {
        type = str;
        default = toString <secrets> + "/${config._module.args.name}";
      };
    };
  });

  suffixed-str = suffs:
    mkOptionType {
      name = "string suffixed by ${concatStringsSep ", " suffs}";
      check = x: isString x && any (flip hasSuffix x) suffs;
      merge = mergeOneOption;
    };

  user = submodule ({ config, ... }: {
    options = {
      mail = mkOption {
        type = str; # TODO retiolum mail address
      };
      name = mkOption {
        type = username;
        default = config._module.args.name;
      };
      pubkey = mkOption {
        type = str;
      };
    };
  });

  # TODO
  addr = str;
  addr4 = str;
  addr6 = str;

  krebs.file-location = types.submodule {
    options = {
      # TODO user
      host = mkOption {
        type = host;
      };
      # TODO merge with ssl.privkey.path
      path = mkOption {
        type = types.either types.path types.str;
        apply = x: {
          path = toString x;
          string = x;
        }.${typeOf x};
      };
    };
  };

  # RFC952, B. Lexical grammar, <hname>
  hostname = mkOptionType {
    name = "hostname";
    check = x: all label.check (splitString "." x);
    merge = mergeOneOption;
  };

  # RFC952, B. Lexical grammar, <name>
  # RFC1123, 2.1  Host Names and Numbers
  label = mkOptionType {
    name = "label";
    # TODO case-insensitive labels
    check = x: match "[0-9A-Za-z]([0-9A-Za-z-]*[0-9A-Za-z])?" x != null;
    merge = mergeOneOption;
  };

  # POSIX.1‐2013, 3.278 Portable Filename Character Set
  filename = mkOptionType {
    name = "POSIX filename";
    check = let
      filename-chars = stringToCharacters
        "-.0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    in s: all (flip elem filename-chars) (stringToCharacters s);
    merge = mergeOneOption;
  };

  # POSIX.1-2013, 3.431 User Name
  username = mkOptionType {
    name = "POSIX username";
    check = s: filename.check s && substring 0 1 s != "-";
  };
}
