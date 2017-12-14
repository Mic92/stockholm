with import <stockholm/lib>;
import <stockholm/lass/source.nix> {
  name = "xerxes";
  secure = true;
  override = {
    nixpkgs.git = mkForce {
      url = https://github.com/lassulus/nixpkgs;
      ref = "3eccd0b";
    };
  };
}
