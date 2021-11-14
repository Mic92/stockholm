{ lib, ... }:

let
  inherit (lib)
    all any attrNames concatMapStringsSep concatStringsSep const filter flip
    genid_uint31 hasSuffix head isInt isString length mergeOneOption mkOption
    mkOptionType optional optionalAttrs optionals range splitString
    stringLength substring test testString typeOf;
  inherit (lib.types)
    attrsOf bool either enum int listOf nullOr path str submodule;
in

rec {

  host = submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = label;
        default = config._module.args.name;
      };
      cores = mkOption {
        type = uint;
      };
      nets = mkOption {
        type = attrsOf net;
        default = {};
      };

      binary-cache.pubkey = mkOption {
        type = nullOr binary-cache-pubkey;
        default = null;
      };

      ci = mkOption {
        description = ''
          If true, then the host wants to be tested by some CI system.
          See ‹stockholm/krebs/2configs/buildbot-all.nix›
        '';
        type = bool;
        default = false;
      };

      external = mkOption {
        description = ''
          Whether the host is defined externally (in contrast to being defined
          in ‹stockholm›).  This is useful e.g. when legacy and/or adopted
          hosts should be part of retiolum or some other component.
        '';
        type = bool;
        default = false;
      };

      monitoring = mkOption {
        description = ''
          Whether the host should be monitored by monitoring tools like Prometheus.
        '';
        type = bool;
        default = false;
      };

      owner = mkOption {
        type = user;
      };

      extraZones = mkOption {
        default = {};
        # TODO: string is either MX, NS, A or AAAA
        type = attrsOf str;
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
      };
      ssh.privkey = mkOption {
        type = nullOr ssh-privkey;
        default = null;
      };

      syncthing.id = mkOption {
        # TODO syncthing id type
        type = nullOr str;
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
        type =
          # XXX break infinite recursion when generating manuals
          if config._module.args.name == "‹name›" then
            mkOptionType {
              name = "‹net›";
            }
          else
            nullOr net;
        default = null;
      };
      addrs = mkOption {
        type = listOf addr;
        default =
          optional (config.ip4 != null) config.ip4.addr ++
          optional (config.ip6 != null) config.ip6.addr;
      };
      aliases = mkOption {
        # TODO nonEmptyListOf hostname
        type = listOf hostname;
        default = [];
      };
      mac = mkOption {
        type = nullOr str;
        default = null;
      };
      ip4 = mkOption {
        type = nullOr (submodule {
          options = {
            addr = mkOption {
              type = addr4;
            };
            prefix = mkOption ({
              type = cidr4;
            } // optionalAttrs (config._module.args.name == "retiolum") {
              default = "10.243.0.0/16";
            } // optionalAttrs (config._module.args.name == "wiregrill") {
              default = "10.244.0.0/16";
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
              apply = lib.normalize-ip6-addr;
            };
            prefix = mkOption ({
              type = cidr6;
            } // optionalAttrs (config._module.args.name == "retiolum") {
              default = "42::/16";
            } // optionalAttrs (config._module.args.name == "wiregrill") {
              default = "42:1::/32";
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
                  (map (a: "Address = ${a} ${toString config.port}") net.via.addrs))
                ++
                (map (a: "Subnet = ${a}") net.addrs)
                ++
                (map (a: "Subnet = ${a}") config.subnets)
                ++
                [config.extraConfig]
                ++
                [config.pubkey]
              );
              defaultText = ''
                Address = ‹addr› ‹port› # for each ‹net.via.addrs›
                Subnet = ‹addr› # for each ‹net.addrs›
                ‹extraConfig›
                ‹pubkey›
              '';
            };
            pubkey = mkOption {
              type = tinc-pubkey;
            };
            pubkey_ed25519 = mkOption {
              type = nullOr tinc-pubkey;
              default = null;
            };
            extraConfig = mkOption {
              description = "Extra Configuration to be appended to the hosts file";
              default = "";
              type = str;
            };
            port = mkOption {
              type = int;
              description = "tinc port to use to connect to host";
              default = 655;
            };
            subnets = mkOption {
              type = listOf cidr;
              description = "tinc subnets";
              default = [];
            };
          };
        }));
        default = null;
      };
      wireguard = mkOption {
        type = nullOr (submodule ({ config, ... }: {
          options = {
            port = mkOption {
              type = int;
              description = "tinc port to use to connect to host";
              default = 51820;
            };
            pubkey = mkOption {
              type = wireguard-pubkey;
            };
            subnets = mkOption {
              type = listOf cidr;
              description = ''
                wireguard subnets,
                this defines how routing behaves for hosts that can't reach each other.
              '';
              default = [];
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
      name = mkOption {
        type = pathname;
        default = config._module.args.name;
      };
      path = mkOption {
        type = absolute-pathname;
        default = "/run/keys/${config.name}";
        defaultText = "/run/keys/‹name›";
      };
      mode = mkOption {
        type = file-mode;
        default = "0400";
      };
      owner = mkOption {
        type = user;
      };
      group-name = mkOption {
        type = str;
        default = "root";
      };
      service = mkOption {
        type = systemd.unit-name;
        default = "secret-${lib.systemd.encodeName config.name}.service";
        defaultText = "secret-‹name›.service";
      };
      source-path = mkOption {
        type = str;
        default = toString <secrets> + "/${config.name}";
        defaultText = "‹secrets/‹name››";
      };
    };
  });


  source = submodule ({ config, ... }: {
    options = {
      type = let
        known-types = attrNames source-types;
        type-candidates = filter (k: config.${k} != null) known-types;
      in mkOption {
        default = if length type-candidates == 1
                    then head type-candidates
                    else throw "cannot determine type";
        type = enum known-types;
      };
      file = mkOption {
        apply = x:
          if absolute-pathname.check x
            then { path = x; }
            else x;
        default = null;
        type = nullOr (either absolute-pathname source-types.file);
      };
      git = mkOption {
        default = null;
        type = nullOr source-types.git;
      };
      pass = mkOption {
        default = null;
        type = nullOr source-types.pass;
      };
      pipe = mkOption {
        apply = x:
          if absolute-pathname.check x
            then { command = x; }
            else x;
        default = null;
        type = nullOr (either absolute-pathname source-types.pipe);
      };
      symlink = mkOption {
        type = nullOr (either pathname source-types.symlink);
        default = null;
        apply = x:
          if pathname.check x
            then { target = x; }
            else x;
      };
    };
  });

  source-types = {
    file = submodule {
      options = {
        path = mkOption {
          type = absolute-pathname;
        };
      };
    };
    git = submodule {
      options = {
        ref = mkOption {
          type = str; # TODO types.git.ref
        };
        url = mkOption {
          type = str; # TODO types.git.url
        };
      };
    };
    pass = submodule {
      options = {
        dir = mkOption {
          type = absolute-pathname;
        };
        name = mkOption {
          type = pathname; # TODO relative-pathname
        };
      };
    };
    pipe = submodule {
      options = {
        command = mkOption {
          type = absolute-pathname;
        };
      };
    };
    symlink = submodule {
      options = {
        target = mkOption {
          type = pathname; # TODO relative-pathname
        };
      };
    };

  };

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
        defaultText = "/home/‹name›";
      };
      mail = mkOption {
        type = nullOr str;
        default = null;
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
        default = genid_uint31 config.name;
        defaultText = "genid_uint31 ‹name›";
      };
    };
  });
  group = submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = username;
        default = config._module.args.name;
        defaultText = "genid_uint31 ‹name›";
      };
      gid = mkOption {
        type = int;
        default = genid_uint31 config.name;
        defaultText = "genid_uint31 ‹name›";
      };
    };
  });

  addr = either addr4 addr6;
  addr4 = mkOptionType {
    name = "IPv4 address";
    check = let
      IPv4address = let d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"; in
        concatMapStringsSep "." (const d) (range 1 4);
    in
      test IPv4address;
    merge = mergeOneOption;
  };
  addr6 = mkOptionType {
    name = "IPv6 address";
    check = let
      # TODO check IPv6 address harder
      IPv6address = "[0-9a-f.:]+";
    in
      test IPv6address;
    merge = mergeOneOption;
  };

  cidr = either cidr4 cidr6;
  cidr4 = mkOptionType {
    name = "CIDRv4 address";
    check = let
      CIDRv4address = let d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"; in
        concatMapStringsSep "." (const d) (range 1 4) + "(/([1-2]?[0-9]|3[0-2]))?";
    in
      test CIDRv4address;
    merge = mergeOneOption;
  };
  cidr6 = mkOptionType {
    name = "CIDRv6 address";
    check = let
      # TODO check IPv6 address harder
      CIDRv6address = "[0-9a-f.:]+(/([0-9][0-9]?|1[0-2][0-8]))?";
    in
      test CIDRv6address;
    merge = mergeOneOption;
  };

  binary-cache-pubkey = str;

  pgp-pubkey = str;

  sitemap.entry = submodule ({ config, ... }: {
    options = {
      desc = mkOption {
        default = null;
        type = nullOr str;
      };
      href = mkOption {
        ${if testString "https?://.*" config._module.args.name
          then "default" else null} = config._module.args.name;
        type = nullOr str; # TODO nullOr uri?
      };
    };
  });

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

  krebs.file-location = submodule {
    options = {
      # TODO user
      host = mkOption {
        type = host;
      };
      # TODO merge with ssl.privkey.path
      path = mkOption {
        type = either path str;
        apply = x: {
          path = toString x;
          string = x;
        }.${typeOf x};
      };
    };
  };

  file-mode = mkOptionType {
    name = "file mode";
    check = test "[0-7]{4}";
    merge = mergeOneOption;
  };

  haskell.conid = mkOptionType {
    name = "Haskell constructor identifier";
    check = test "[[:upper:]][[:lower:]_[:upper:]0-9']*";
    merge = mergeOneOption;
  };

  haskell.modid = mkOptionType {
    name = "Haskell module identifier";
    check = x: isString x && all haskell.conid.check (splitString "." x);
    merge = mergeOneOption;
  };

  systemd.unit-name = mkOptionType {
    name = "systemd unit name";
    check = x:
      test "^[0-9A-Za-z:_.\\-]+@?\\.(service|socket|device|mount|automount|swap|target|path|timer|slice|scope)$" x &&
      stringLength x <= 256;
    merge = mergeOneOption;
  };

  # RFC952, B. Lexical grammar, <hname>
  hostname = mkOptionType {
    name = "hostname";
    check = x: isString x && all label.check (splitString "." x);
    merge = mergeOneOption;
  };

  # RFC952, B. Lexical grammar, <name>
  # RFC1123, 2.1  Host Names and Numbers
  label = mkOptionType {
    name = "label";
    # TODO case-insensitive labels
    check = test "[0-9A-Za-z]([0-9A-Za-z-]*[0-9A-Za-z])?";
    merge = mergeOneOption;
  };

  # POSIX.1‐2017, 3.190 Group Name
  groupname = mkOptionType {
    name = "POSIX group name";
    check = filename.check;
    merge = mergeOneOption;
  };

  # POSIX.1‐2017, 3.281 Portable Filename
  filename = mkOptionType {
    name = "POSIX portable filename";
    check = test "[0-9A-Za-z._][0-9A-Za-z._-]*";
    merge = mergeOneOption;
  };

  # POSIX.1‐2017, 3.2 Absolute Pathname
  absolute-pathname = mkOptionType {
    name = "POSIX absolute pathname";
    check = x: isString x && substring 0 1 x == "/" && pathname.check x;
    merge = mergeOneOption;
  };

  # POSIX.1-2017, 3.271 Pathname
  pathname = mkOptionType {
    name = "POSIX pathname";
    check = x:
      let
        # The filter is used to normalize paths, i.e. to remove duplicated and
        # trailing slashes.  It also removes leading slashes, thus we have to
        # check for "/" explicitly below.
        xs = filter (s: stringLength s > 0) (splitString "/" x);
      in
        isString x && (x == "/" || (length xs > 0 && all filename.check xs));
    merge = mergeOneOption;
  };

  # POSIX.1-2017, 3.216 Login Name
  username = mkOptionType {
    name = "POSIX login name";
    check = filename.check;
    merge = mergeOneOption;
  };

  wireguard-pubkey = str;
}
