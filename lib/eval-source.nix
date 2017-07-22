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
  sanitize = x: getAttr (typeOf x) {
    set = mapAttrs
            (const sanitize)
            (filterAttrs (name: value: name != "_module" && value != null) x);
    string = x;
  };
in
  # This function's return value can be used as pkgs.populate input.
  _file: source: sanitize (eval _file source).config.source
