{ pkgs }:

pkgs.flameshot.overrideAttrs (old: rec {
  name = "flameshot-${version}";
  version = "12.1.0-pre";
  src = pkgs.fetchFromGitHub {
    owner = "flameshot-org";
    repo = "flameshot";
    rev = "f7e41f4d708e50eeaec892408069da25a28e04a2";
    hash = "sha256-fZquXY0xSaN1hJgCh16MocIlvxHe1c2Nt+fGF2NIOVw=";
  };
  patches = old.patches or [] ++ [
    ./flameshot-12.imgur.patch
  ];
})
