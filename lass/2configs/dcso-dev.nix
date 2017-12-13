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
        config.krebs.users.lass.pubkey
        config.krebs.users.lass-android.pubkey
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDhQdDQFMxXOjbC+Avx3mlcFHqQpFUk/q9sO6ATA65jCV3YzN11vhZDDv54hABVS2h8TPXs7Lu3PCvK9qouASd2h4Ie9cExUmn50G/iwgFIODsCugVYBzVt1iwaAdwz1Hb9DKYXbVXanzVJjimmrrlQNvsyZg85lcnfyedpPX5ad+4FdSP68LHqEHC18LTitldR6V4P1omaKHlOtVpDgR/72tDgbtNZDBn3EU+TPk9OLTzjc6PinPw4iIvjEfiu14APwXpFDIqT7P7SjOEFpa0v/1z7dhxIy/Z9XbqyEdUfhv3PjZR5K2C+VzR7g6jVEVR2xFId51MpLv/Un4/lalbphBEw3I90Rr8tatOJiFhyrXbaKTcLqp1sIu05OxdPkm3hzfmLIhoKxhaIlXH7WQ9sAqxL1NAQ7O+J6yT4DMnwKzvpkkJjBaGtV84Pp1cccfNRH8XXID3FkWkrUpdgXWBpyLnRq4ilUJTajkU0GSdXkq8kLL3mWg9LPRTg3dmDj61ZB/qhjM61ppwHJvDRN9WI5HruXIU6nOQjh5yE2C/JZfLcsZD4Y1UDBy5/JSZrCVT2sQjFopkkYEkRCbX7oITHOH4iyRdxZkKWLUPboFrcmBpXO+owCEhO4JZrtfFWMC6qM++nrmiZWOrdIOIvdYHWluhKR2shlkisEKQP5pUqkw== markus.hihn@dcso.de"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1T5+2epslFARSnETdr4wdolA6ocJaD4H9tmz6BZFQKXlwIq+OMp+sSEdwYwW3Lu9+mNbBHPxVVJDWg/We9DXB0ezXPM5Bs1+FcehmkoGwkmgKaFCDt0sL+CfSnog/3wEkN21O/rQxVFqMmiJ7WUDGci6IKCFZ5ZjOsmmfHg5p3LYxU9xv33fNr2v+XauhrGbFtQ7eDz4kSywxN/aw73LN4d8em0V0UV8VPI3Qkw7MamDFwefA+K1TfK8pBzMeruU6N7HLuNkpkAp7kS+K4Zzd72aQtR37a5qMiFUbOxQ9B7iFypuPx0iu6ZwY1s/sM8t3kLmcDJ9O4FOTzlbpneet3as6iJ+Ckr/TlfKor2Tl5pWcXh2FXHoG8VUu5bYmIViJBrKihAlAQfQN0mJ9fdFTnCXVTtbYTy11s4eEVHgUlb7oSpgBnx5bnBONgApbsOX9zyoo8wz8KkZBcf1SQpkV5br8uUAHCcZtHuY6I3kKlv+8lJmgUipiYzMdTi7+dHa49gVEcEKL4ZnJ0msQkl4XT7JjKETLvumC4/TIqVuRu48wuYalkCR9OzxCsTXQ/msBJBztPdYLrEOXVb2HfzuCT+43UuMQ5rP/EoPy0TWQO9BaqfEXqvbOvWjVxj/GMvglQ2ChZTwHxwwTKB8qRVvJLnbZQwizQiSrkzjb6hRJfQ== u0_a165@localhost"
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
