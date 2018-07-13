{ config, lib, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
  inherit (import <stockholm/lib>) genid;

in {
  users.extraUsers = {
    dev = {
      name = "dev";
      uid = genid "dev";
      extraGroups = [ "docker" "vboxusers" ];
      description = "user for collaborative development";
      home = "/home/dev";
      useDefaultShell = true;
      createHome = true;
      openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
        config.krebs.users.lass-android.pubkey
        config.krebs.users.lass-mors.pubkey
        config.krebs.users.jeschli-bln.pubkey
        config.krebs.users.jeschli-brauerei.pubkey
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1T5+2epslFARSnETdr4wdolA6ocJaD4H9tmz6BZFQKXlwIq+OMp+sSEdwYwW3Lu9+mNbBHPxVVJDWg/We9DXB0ezXPM5Bs1+FcehmkoGwkmgKaFCDt0sL+CfSnog/3wEkN21O/rQxVFqMmiJ7WUDGci6IKCFZ5ZjOsmmfHg5p3LYxU9xv33fNr2v+XauhrGbFtQ7eDz4kSywxN/aw73LN4d8em0V0UV8VPI3Qkw7MamDFwefA+K1TfK8pBzMeruU6N7HLuNkpkAp7kS+K4Zzd72aQtR37a5qMiFUbOxQ9B7iFypuPx0iu6ZwY1s/sM8t3kLmcDJ9O4FOTzlbpneet3as6iJ+Ckr/TlfKor2Tl5pWcXh2FXHoG8VUu5bYmIViJBrKihAlAQfQN0mJ9fdFTnCXVTtbYTy11s4eEVHgUlb7oSpgBnx5bnBONgApbsOX9zyoo8wz8KkZBcf1SQpkV5br8uUAHCcZtHuY6I3kKlv+8lJmgUipiYzMdTi7+dHa49gVEcEKL4ZnJ0msQkl4XT7JjKETLvumC4/TIqVuRu48wuYalkCR9OzxCsTXQ/msBJBztPdYLrEOXVb2HfzuCT+43UuMQ5rP/EoPy0TWQO9BaqfEXqvbOvWjVxj/GMvglQ2ChZTwHxwwTKB8qRVvJLnbZQwizQiSrkzjb6hRJfQ== u0_a165@localhost"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCjtdqRxD0+UU7O8xogSqAQYd/Hrc79CTTKnvbhKy7jp2TVfxQpl81ndSH6DN6Cz90mu65C+DFGq43YtKTPqXmTn1+2wru71C2UOl6ZR0tmU7UELkRt4SJuFQLEgQCt3BWvXJPye6cKRRIlb+XZHWyVyCDxHo9EYO2GWI1wIP8mHMltKj65mobHY+R0CJNhhwlFURzTto8C30ejfVg2OW81qkNWqYtpdC9txLUlQ9/LBVKrafHGprmcBEp9qtecVgx8kxHpS7cuQNYoFcfljug4IyFO+uBfdbKqnGM5mra3huNhX3+AcQxKbLMlRgZD+jc47Xs+s5qSvWBou2ygd5T413k/SDOTCxDjidA+dcwzRo0qUWcGL201a5g+F0EvWv8rjre9m0lii6QKEoPyj60y3yfaIHeafels1Ia1FItjkBe8XydiXf7rKq8nmVRlpo8vl+vKwVuJY783tObHjUgBtXJdmnyYGiXxkxSrXa2mQhPz3KodK/QrnqCP27dURcMlp1hFF3LxFz7WtMCLW0yvDuUsuI2pdq0+zdt702wuwXVNIvbq/ssvX/CL8ryBLAogaxN9DN0vpjk+aXQLn11Zt99MgmnnqUgvOKQi1Quog/SxnSBiloKqB6aA10a28Uxoxkr0KAfhWhX3XPpfGMlbVj4GJuevLp0sGDVQT2biUQ== rhaist@RH-NB"
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

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 8000"; target = "ACCEPT";}
    { predicate = "-p tcp --dport 9000"; target = "ACCEPT";}
  ];

  krebs.per-user.dev.packages = [
    pkgs.go
  ];
  environment.variables.GOPATH = "$HOME/go";

  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(dev) NOPASSWD: ALL
  '';

  networking.interfaces.et0.ipv4.addresses = [
    { address = "10.99.23.1"; prefixLength = 24; }
  ];
  virtualisation.docker.enable = true;
  environment.etc."docker/daemon.json".source = pkgs.writeText "daemon.json" ''
    {
      "bip": "172.25.0.1/16"
    }
  '';
  services.rabbitmq.enable = true;
}
