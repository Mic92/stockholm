with import ./lib;
{ config, ... }: {
  options.org.freedesktop.machine1.host-shell.access = lib.mkOption {
    default = {};
    type =
      lib.types.addCheck
        (lib.types.attrsOf (lib.types.attrsOf lib.types.bool))
        (x:
          lib.all
            lib.types.username.check
            (lib.concatLists
              (lib.mapAttrsToList
                (name: value: [name] ++ lib.attrNames value)
                x)));
  };
  config.security.polkit.extraConfig = let
    cfg = config.org.freedesktop.machine1.host-shell;
    enable = cfg.access != {};
  in lib.optionalString enable /* js */ ''
    polkit.addRule(function () {
      var access = ${lib.toJSON cfg.access};
      return function(action, subject) {
        if (action.id === "org.freedesktop.machine1.host-shell"
            && (access[subject.user]||{})[action.lookup("user")])
          return polkit.Result.YES;
      }
    }());
  '';
}
