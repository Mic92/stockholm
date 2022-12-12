with import ./lib;
let
  local.types.screen = lib.types.submodule {
    options.width = lib.mkOption {
      type = lib.types.uint;
    };
    options.height = lib.mkOption {
      type = lib.types.uint;
    };
  };
in {
  options.tv.hw.screens = lib.mkOption {
    type = lib.types.attrsOf local.types.screen;
    default = {};
  };
}
