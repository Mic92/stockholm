with import <stockholm/lib>;
evalSource "" [{
  nixos-config = {
    symlink.target = toString ./test-config;
  };
  nixpkgs = {
    symlink.target = toString <nixpkgs>;
  };
  stockholm = {
    symlink.target = toString <stockholm>;
  };
}]
