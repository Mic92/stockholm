self: super:

super.alacritty.overrideAttrs (old:
  assert self.lib.versions.majorMinor old.version == "0.11";
  {
    version = "${old.version}-tv";
    src = self.fetchFromGitHub {
      owner = "4z3";
      repo = "alacritty";
      rev = "touchscreen-support-0.11";
      hash = "sha256-oA4earrJ7lPVSBm9vRccWatAQ49hfDKsa7M72B5uQpY=";
    };
  }
)
