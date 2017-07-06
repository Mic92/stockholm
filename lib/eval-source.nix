with import <stockholm/lib>;
let
  eval = _file: source: evalModules {
    modules = singleton {
      inherit _file;
      options.source = mkOption {
        type = types.attrsOf types.source;
        default = {};
      };
      config = {
        inherit source;
      };
    };
  };
in
  # This function's return value can be used as pkgs.populate input.
  _file: source: (eval _file source).config.source
