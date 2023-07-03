self: super:

super.alacritty.overrideAttrs (old:
  if self.lib.versions.majorMinor old.version == "0.12" then
    {
      version = "${old.version}-tv";
      src = self.fetchFromGitHub {
        owner = "4z3";
        repo = "alacritty";
        rev = "touchscreen-support-0.12";
        hash = "sha256-yDG7IeQUmJhKMJebhMDzHLb3UHGLcO1FVZnmGe5Xr9w=";
      };
    }
  else
    builtins.trace "not overriding alacritty because unsupported version" {}
)
