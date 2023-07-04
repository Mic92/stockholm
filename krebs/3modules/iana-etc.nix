{ config, lib, pkgs, ... }: let
  slib = import ../../lib/pure.nix { inherit lib; };
in with lib; {

  options.krebs.iana-etc.services = mkOption {
    default = {};
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        port = mkOption {
          default = config._module.args.name;
          type = types.addCheck types.str (slib.test "[1-9][0-9]*");
        };
      } // genAttrs ["tcp" "udp"] (protocol: mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            name = mkOption {
              type = types.str;
            };
          };
        });
      });
    }));
  };

  config.environment.etc = mkIf (config.krebs.iana-etc.services != {})  {
    services.source = mkForce (pkgs.runCommand "krebs-iana-etc" {} /* sh */ ''
      {
        ${concatMapStringsSep "\n" (entry: /* sh */ ''
          ${concatMapStringsSep "\n"
            (proto: let
              line = "${entry.${proto}.name} ${entry.port}/${proto}";
            in /* sh */ ''
              echo ${slib.shell.escape line}
            '')
            (filter (proto: entry.${proto} != null) ["tcp" "udp"])}
          '') (attrValues config.krebs.iana-etc.services)}
        cat ${pkgs.iana-etc}/etc/services
      } |
      sort -b -k 2,2 -u > $out
    '');
  };

}
