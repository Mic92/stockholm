{ pkgs, ... }:

(pkgs.haskellngPackages.override {
  overrides = self: super: {
    lentil = super.lentil.override {
      mkDerivation = (attrs: self.mkDerivation (attrs // {
        version = "0.1.2.7";
        sha256 = "1g3if2y41li6wyg7ffvpybqvbywiq8bf5b5fb6pz499hinzahb9d";
        patches = [
          ./1.patch
        ];
        doCheck = false;
      }));
    };
  };
}).lentil
