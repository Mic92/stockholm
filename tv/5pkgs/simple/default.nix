with import ../../../lib;

self: super:

let
  # This callPackage will try to detect obsolete overrides.
  callPackage = path: args: let
    override = self.callPackage path args;
    upstream = optionalAttrs (override ? "name")
      (super.${(parseDrvName override.name).name} or {});
  in if upstream ? "name" &&
        override ? "name" &&
        compareVersions upstream.name override.name != -1
    then trace "Upstream `${upstream.name}' gets overridden by `${override.name}'." override
    else override;
in

  mapNixDir (path: callPackage path {}) ./.
