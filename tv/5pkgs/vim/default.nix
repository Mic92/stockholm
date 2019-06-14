with import <stockholm/lib>;

self: super: {
  tv = super.tv // {
    vimPlugins = mapNixDir (path: self.callPackage path {}) ./.;
  };
}
