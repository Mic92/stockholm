with import <stockholm/lib>;
self: super: let

  # This callPackage will try to detect obsolete overrides.
  callPackage = path: args: let
    override =  super.callPackage path args;
    upstream = optionalAttrs (override ? "name")
      (super.${(parseDrvName override.name).name} or {});
  in if upstream ? "name" &&
        override ? "name" &&
        compareVersions upstream.name override.name != -1
    then
      trace
        "Upstream `${upstream.name}' gets overridden by `${override.name}'."
        override
    else override;

   subdirsOf = path:
     mapAttrs (name: _: path + "/${name}")
              (filterAttrs (_: eq "directory") (readDir path));

in mapAttrs (_: flip callPackage {})
            (filterAttrs (_: dir: pathExists (dir + "/default.nix"))
                         (subdirsOf ./.))
