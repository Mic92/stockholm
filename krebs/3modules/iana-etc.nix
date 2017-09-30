with import <stockholm/lib>;
{ config, pkgs, ... }: {

  options.krebs.iana-etc.services = mkOption {
    default = {};
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        port = mkOption {
          default = config._module.args.name;
          type = types.addCheck types.str (test "[1-9][0-9]*");
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
    services.source = mkForce (pkgs.runCommand "krebs-iana-etc" {} ''
      exec < ${pkgs.iana_etc}/etc/services
      exec > $out
      awk -F '[ /]+' '
        BEGIN {
          port=0
        }
        ${concatMapStringsSep "\n" (entry: ''
          $2 == ${entry.port} {
            port=$2
            next
          }
          port == ${entry.port} {
            ${concatMapStringsSep "\n"
              (proto: let
                s = "${entry.${proto}.name} ${entry.port}/${proto}";
              in
                "print ${toJSON s}")
              (filter (proto: entry.${proto} != null) ["tcp" "udp"])}
            port=0
          }
        '') (attrValues config.krebs.iana-etc.services)}
        {
          print $0
        }
      '
    '');
  };

}
