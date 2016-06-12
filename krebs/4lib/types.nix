{ config, lib, ... }:

with builtins;
with lib;
with types;

let
  # Inherited attributes are used in submodules that have their own `config`.
  inherit (config.krebs) build users;
in

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
        default = users.krebs;
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
        type = nullOr ssh-pubkey;
        default = null;
        apply = x:
          optionalTrace (x == null && config.owner.name == build.user.name)
            "The option `krebs.hosts.${config.name}.ssh.pubkey' is unused."
            x;
      };
      ssh.privkey = mkOption {
        type = nullOr ssh-privkey;
        default = null;
      };
    };
  });

  net = submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = label;
        default = config._module.args.name;
      };
      via = mkOption {
        type = nullOr net;
        default = null;
      };
      addrs = mkOption {
        type = listOf addr;
        default =
          optional (config.ip4 != null) config.ip4.addr ++
          optional (config.ip6 != null) config.ip6.addr;
        readOnly = true;
      };
      aliases = mkOption {
        # TODO nonEmptyListOf hostname
        type = listOf hostname;
        default = [];
      };
      ip4 = mkOption {
        type = nullOr (submodule {
          options = {
            addr = mkOption {
              type = addr4;
            };
            prefix = mkOption ({
              type = str; # TODO routing prefix (CIDR)
            } // optionalAttrs (config.name == "retiolum") {
              default = "10.243.0.0/16";
            });
          };
        });
        default = null;
      };
      ip6 = mkOption {
        type = nullOr (submodule {
          options = {
            addr = mkOption {
              type = addr6;
            };
            prefix = mkOption ({
              type = str; # TODO routing prefix (CIDR)
            } // optionalAttrs (config.name == "retiolum") {
              default = "42::/16";
            });
          };
        });
        default = null;
      };
      ssh = mkOption {
        type = submodule {
          options = {
            port = mkOption {
              type = int;
              default = 22;
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
              type = tinc-pubkey;
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

  uint = mkOptionType {
    name = "unsigned integer";
    check = x: isInt x && x >= 0;
    merge = mergeOneOption;
  };

  secret-file = submodule ({ config, ... }: {
    options = {
      path = mkOption { type = str; };
      mode = mkOption { type = file-mode; default = "0400"; };
      owner = mkOption {
        type = user;
        default = config.krebs.users.root;
      };
      group-name = mkOption {
        type = str;
        default = "root";
      };
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
      home = mkOption {
        type = absolute-pathname;
        default = "/home/${config.name}";
      };
      mail = mkOption {
        type = str; # TODO retiolum mail address
      };
      name = mkOption {
        type = username;
        default = config._module.args.name;
      };
      pgp.pubkeys = mkOption {
        type = attrsOf pgp-pubkey;
        default = {};
        description = ''
          Set of user's PGP public keys.

          Modules supporting PGP may use well-known key names to define
          default values for options, in which case the well-known name
          should be documented in the respective option's description.
        '';
      };
      pubkey = mkOption {
        type = nullOr ssh-pubkey;
        default = null;
      };
      uid = mkOption {
        type = int;
        default = genid config.name;
      };
    };
  });
  group = submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = username;
        default = config._module.args.name;
      };
      gid = mkOption {
        type = int;
        default = genid config.name;
      };
    };
  });

  addr = either addr4 addr6;
  addr4 = mkOptionType {
    name = "IPv4 address";
    check = let
      IPv4address = let d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"; in
        concatMapStringsSep "." (const d) (range 1 4);
    in x: match IPv4address x != null;
    merge = mergeOneOption;
  };
  addr6 = mkOptionType {
    name = "IPv6 address";
    check = let
      # TODO check IPv6 address harder
      IPv6address = "[0-9a-f.:]+";
    in x: match IPv6address x != null;
    merge = mergeOneOption;
  };

  pgp-pubkey = str;

  ssh-pubkey = str;
  ssh-privkey = submodule {
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
  };

  tinc-pubkey = str;

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

  file-mode = mkOptionType {
    name = "file mode";
    check = x: isString x && match "[0-7]{4}" x != null;
    merge = mergeOneOption;
  };

  haskell.conid = mkOptionType {
    name = "Haskell constructor identifier";
    check = x:
      isString x && match "[[:upper:]][[:lower:]_[:upper:]0-9']*" x != null;
    merge = mergeOneOption;
  };

  haskell.modid = mkOptionType {
    name = "Haskell module identifier";
    check = x: isString x && all haskell.conid.check (splitString "." x);
    merge = mergeOneOption;
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
    check = x: match "([0-9A-Za-z._])[0-9A-Za-z._-]*" x != null;
    merge = mergeOneOption;
  };

  # POSIX.1‐2013, 3.2 Absolute Pathname
  # TODO normalize slashes
  # TODO two slashes
  absolute-pathname = mkOptionType {
    name = "POSIX absolute pathname";
    check = x: let xs = splitString "/" x; xa = head xs; in
      xa == "/" || (xa == "" && all filename.check (tail xs));
    merge = mergeOneOption;
  };

  # POSIX.1‐2013, 3.267 Pathname
  # TODO normalize slashes
  pathname = mkOptionType {
    name = "POSIX pathname";
    check = x: let xs = splitString "/" x; in
      all filename.check (if head xs == "" then tail xs else xs);
    merge = mergeOneOption;
  };

  # POSIX.1-2013, 3.431 User Name
  username = mkOptionType {
    name = "POSIX username";
    check = filename.check;
    merge = mergeOneOption;
  };
}
