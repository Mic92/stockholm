self: super:

super.flameshot.overrideAttrs (old: rec {
  name = "flameshot-${version}";
  version = "0.10.2";
  src = self.fetchFromGitHub {
    owner = "flameshot-org";
    repo = "flameshot";
    rev = "v${version}";
    sha256 = "sha256-rZUiaS32C77tFJmEkw/9MGbVTVscb6LOCyWaWO5FyR4=";
  };
  patches = old.patches or [] ++ [
    ./flameshot/flameshot_imgur_0.10.2.patch
  ];
})
