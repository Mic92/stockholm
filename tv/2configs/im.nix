{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
{
  environment.systemPackages = with pkgs; [
    (pkgs.writeDashBin "im" ''
      export PATH=${makeSearchPath "bin" (with pkgs; [
        tmux
        gnugrep
        weechat
      ])}
      if tmux list-sessions -F\#S | grep -q '^im''$'; then
        exec tmux attach -t im
      else
        exec tmux new -s im weechat
      fi
    '')
  ];
  services.bitlbee = {
    enable = true;
    plugins = [
      pkgs.bitlbee-facebook
    ];
  };
}
