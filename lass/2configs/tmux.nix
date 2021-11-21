with import <stockholm/lib>;
{ config, pkgs, ... }:

{
  nixpkgs.config.packageOverrides = super: {
    tmux = pkgs.symlinkJoin {
      name = "tmux";
      paths = [
        (pkgs.writeDashBin "tmux" ''
          exec ${super.tmux}/bin/tmux -f ${pkgs.writeText "tmux.conf" ''
            #change prefix key to `
            set-option -g prefix `
            unbind-key C-b
            bind ` send-prefix

            set-option -g default-terminal screen-256color

            #use session instead of windows
            bind-key c new-session
            bind-key p switch-client -p
            bind-key n switch-client -n
            bind-key C-s switch-client -l
          ''} "$@"
        '')
        super.tmux
      ];
    };
  };
  environment.systemPackages = with pkgs; [
    tmux
  ];

  # programs.bash.interactiveShellInit = ''
  #   if [[ "$TERM" != "linux" && -z "$TMUX" ]]; then
  #     if [[ -n "$SSH_AUTH_SOCK" ]]; then
  #       tmux set-environment -g SSH_AUTH_SOCK "$SSH_AUTH_SOCK" 2>/dev/null
  #     fi

  #     exec tmux -u
  #   fi
  #   if [[ "$__host__" != "$HOST" ]]; then
  #     tmux set -g status-bg colour$(string_hash $HOST 255)
  #     export __host__=$HOST
  #   fi
  # '';
}
