with import <stockholm/lib>;
let
  overrides = self: super:
    mapNixDir (path: self.callPackage path {}) [
      <stockholm/krebs/5pkgs/haskell>
      ./.
    ];
in
  self: super: {
    haskell = super.haskell // {
      packages = mapAttrs (name: value:
        if hasAttr "override" value
          then value.override { inherit overrides; }
          else value
      ) super.haskell.packages;
    };
    haskellPackages = super.haskellPackages.override {
      inherit overrides;
    };
  }
