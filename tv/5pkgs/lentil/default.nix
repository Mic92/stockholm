{ pkgs, ... }:

(pkgs.haskellngPackages.override {
  overrides = self: super: {
    lentil = super.lentil.override {
      mkDerivation = (attrs: self.mkDerivation (attrs // {
        version = "0.1.3.0";
        sha256 = "0xa59avh0bvfg69xh9p5b8dppfhx29mvfq8v41sk9j7qbcnzjivg";
        patches = [
          ./syntaxes.patch
        ];
      }));
    };
  };
}).lentil
