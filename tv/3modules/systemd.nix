with import ./lib;
{ config, ... }: let
  normalUsers = filterAttrs (_: getAttr "isNormalUser") config.users.users;
in {
  options = {
    tv.systemd.services = mkOption {
      type = types.attrsOf (types.submodule (self: {
        options = {
          operators = mkOption {
            type = with types; listOf (enum (attrNames normalUsers));
            default = [];
          };
        };
      }));
      default = {};
    };
  };
  config = {
    security.polkit.extraConfig = let
      access =
        mapAttrs'
          (name: cfg:
            nameValuePair "${name}.service"
                          (genAttrs cfg.operators (const true))
          )
          config.tv.systemd.services;
    in optionalString (access != {}) /* js */ ''
      polkit.addRule(function () {
        const access = ${lib.toJSON access};
        return function (action, subject) {
          if (action.id === "org.freedesktop.systemd1.manage-units") {
            const unit = action.lookup("unit");
            if (
              (access[unit]||{})[subject.user] ||
              (
                unit.includes("@") &&
                (access[unit.replace(/@[^.]+/, "@")]||{})[subject.user]
              )
            ) {
              return polkit.Result.YES;
            }
          }
        }
      }());
    '';
  };
}
