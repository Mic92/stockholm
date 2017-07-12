with import <stockholm/lib>;
let
  eval = _file: source: evalModules {
    modules = singleton {
      inherit _file;
      imports = map (source: { inherit source; }) (toList source);
      options.source = mkOption {
        type = types.attrsOf types.source;
        default = {};
      };
    };
  };
in
  # This function's return value can be used as pkgs.populate input.
  _file: source: (eval _file source).config.source
