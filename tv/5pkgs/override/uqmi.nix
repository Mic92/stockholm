self: super:

super.uqmi.overrideAttrs (old: {
  version = "unstable-2022-05-04";
  src = self.fetchgit {
    url = "https://git.openwrt.org/project/uqmi.git";
    rev = "56cb2d4056fef132ccf78dfb6f3074ae5d109992";
    hash = "sha256-PwnR24PbNKfLrsBlU5JTOHDzs/9Wgcuwfnu3dJuuZcM=";
  };
})
