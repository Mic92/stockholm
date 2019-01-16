{ pkgs, ... }: {

  services.acpid.enable = true;
  services.acpid.handlers.xkiller = {
    action = /* sh */ ''
      event=($1)
      if test "''${event[2]}" = 00000080; then
        ${pkgs.systemd}/bin/systemd-cat -t xkiller ${pkgs.xkiller}
      fi
    '';
    event = "button/prog1";
  };

}
