{ config, lib, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
  inherit (import <stockholm/lib>) genid;

in {
  users.extraUsers = {
    dev = {
      name = "dev";
      uid = genid "dev";
      description = "user for collaborative development";
      home = "/home/dev";
      useDefaultShell = true;
      createHome = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDhQdDQFMxXOjbC+Avx3mlcFHqQpFUk/q9sO6ATA65jCV3YzN11vhZDDv54hABVS2h8TPXs7Lu3PCvK9qouASd2h4Ie9cExUmn50G/iwgFIODsCugVYBzVt1iwaAdwz1Hb9DKYXbVXanzVJjimmrrlQNvsyZg85lcnfyedpPX5ad+4FdSP68LHqEHC18LTitldR6V4P1omaKHlOtVpDgR/72tDgbtNZDBn3EU+TPk9OLTzjc6PinPw4iIvjEfiu14APwXpFDIqT7P7SjOEFpa0v/1z7dhxIy/Z9XbqyEdUfhv3PjZR5K2C+VzR7g6jVEVR2xFId51MpLv/Un4/lalbphBEw3I90Rr8tatOJiFhyrXbaKTcLqp1sIu05OxdPkm3hzfmLIhoKxhaIlXH7WQ9sAqxL1NAQ7O+J6yT4DMnwKzvpkkJjBaGtV84Pp1cccfNRH8XXID3FkWkrUpdgXWBpyLnRq4ilUJTajkU0GSdXkq8kLL3mWg9LPRTg3dmDj61ZB/qhjM61ppwHJvDRN9WI5HruXIU6nOQjh5yE2C/JZfLcsZD4Y1UDBy5/JSZrCVT2sQjFopkkYEkRCbX7oITHOH4iyRdxZkKWLUPboFrcmBpXO+owCEhO4JZrtfFWMC6qM++nrmiZWOrdIOIvdYHWluhKR2shlkisEKQP5pUqkw== markus.hihn@dcso.de"
      ];
      packages = with pkgs; [
        emacs25-nox

        (pkgs.symlinkJoin {
          name = "tmux";
          paths = [
            (pkgs.writeDashBin "tmux" ''
              exec ${pkgs.tmux}/bin/tmux -f ${pkgs.writeText "tmux.conf" ''
                set-option -g default-terminal screen-256color

                #use session instead of windows
                bind-key c new-session
                bind-key p switch-client -p
                bind-key n switch-client -n
                bind-key C-s switch-client -l
              ''} "$@"
            '')
            pkgs.tmux
          ];
        })
      ];
    };
  };

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(dev) NOPASSWD: ALL
  '';
}
