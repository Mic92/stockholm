with (import <stockholm/lib>);
{ config, lib, pkgs, ... }: let
  weechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: with pkgs.weechatScripts; {
      plugins = lib.attrValues (availablePlugins // {
        python = availablePlugins.python.withPackages (_: [ weechat-matrix ]);
      });
      scripts = [ weechat-matrix ];
    };
  };

  tmux = pkgs.writeDashBin "tmux" ''
    exec ${pkgs.tmux}/bin/tmux -f ${pkgs.writeText "tmux.conf" ''
      set-option -g prefix `
      unbind-key C-b
      bind ` send-prefix

      set-option -g status off
      set-option -g default-terminal screen-256color

      #use session instead of windows
      bind-key c new-session
      bind-key p switch-client -p
      bind-key n switch-client -n
      bind-key C-s switch-client -l
    ''} "$@"
  '';

in {
  imports = [
    ./bitlbee.nix
  ];
  environment.systemPackages = [ tmux weechat ];
  systemd.services.chat = {
    description = "chat environment setup";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    restartIfChanged = false;

    path = [
      pkgs.rxvt_unicode.terminfo
    ];

    serviceConfig = {
      User = "lass";
      RemainAfterExit = true;
      Type = "oneshot";
      ExecStart = "${tmux}/bin/tmux -2 new-session -d -s IM ${weechat}/bin/weechat";
      ExecStop = "${tmux}/bin/tmux kill-session -t IM"; # TODO run save in weechat
    };
  };
}
