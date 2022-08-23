self: super:

let
  version = "1.21.0";
in

# Prevent downgrades.
assert self.lib.versionAtLeast version super.jc.version;

self.python3.pkgs.toPythonApplication
  (self.python3.pkgs.jc.overrideAttrs
    (oldAttrs: {
      name = "jc-${version}";
      version = version;
      src = self.fetchFromGitHub {
        owner = "kellyjonbrazil";
        repo = "jc";
        rev = "v${version}";
        sha256 = "sha256-kS42WokR7ZIqIPi8LbX4tmtjn37tckea2ELbuqzTm2o";
      };
    }))
