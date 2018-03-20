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

in {
    bank = self.writeDashBin "bank" ''
      tmp=$(mktemp)
      ${self.pass}/bin/pass show hledger > $tmp
      ${self.hledger}/bin/hledger --file=$tmp "$@"
      ${self.pass}/bin/pass show hledger | if ${self.diffutils}/bin/diff $tmp -; then
        exit 0
      else
        ${self.coreutils}/bin/cat $tmp | ${self.pass}/bin/pass insert -m hledger
      fi
      ${self.coreutils}/bin/rm $tmp
    '';
    rtl8814au = callPackage ./custom/rtl8814au { kernel = self.linux; };
}

// mapAttrs (_: flip callPackage {})
            (filterAttrs (_: dir: pathExists (dir + "/default.nix"))
                         (subdirsOf ./.))
