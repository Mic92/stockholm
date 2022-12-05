with import <stockholm/lib>;

self: super: {
  tv = super.tv // {
    vim = {
      makePlugin = outPath: outPath // { inherit outPath; };
      makeRuntimePath = concatMapStringsSep "," (getAttr "outPath");
    };
    vimPlugins = mapNixDir (path: self.callPackage path {}) ./.;
  };
}
