{ config, lib, pkgs, ... }: let

  setupGit = ''
    export PATH=${lib.makeBinPath [
      pkgs.coreutils
      pkgs.git
    ]}
    export GIT_SSH_COMMAND='${pkgs.openssh}/bin/ssh -i /var/lib/radicale/.ssh/id_ed25519'
    repo='git@localhost:cal'
    cd /var/lib/radicale/collections
    if ! test -d .git; then
      git init
      git config user.name "radicale"
      git config user.email "radicale@${config.networking.hostName}"
    elif ! url=$(git config remote.origin.url); then
      git remote add origin "$repo"
    elif test "$url" != "$repo"; then
      git remote set-url origin "$repo"
    fi
    cp ${pkgs.writeText "gitignore" ''
      .Radicale.cache
    ''} .gitignore
    git add .gitignore
  '';

  pushCal = pkgs.writeDash "push_cal" ''
    ${setupGit}
    git fetch origin
    git merge --ff-only origin/master || :
  '';

  pushCgit = pkgs.writeDash "push_cgit" ''
    ${setupGit}
    git push origin master
  '';

in {
  services.radicale = {
    enable = true;
    rights = {
      krebs = {
        user = ".*";
        collection = ".*";
        permissions = "rRwW";
      };
    };
    settings = {
      auth.type = "none";
      server.hosts = [
        "0.0.0.0:5232"
        "[::]:5232"
      ];
      storage.filesystem_folder = "/var/lib/radicale/collections";
      storage.hook = "${pkgs.writers.writeDash "radicale-hook" ''
        set -efu
        ${setupGit}
        ${pkgs.git}/bin/git add -A
        (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m "Changes by \"$1\"")
        ${pushCgit}
      ''} %(user)s";
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts = {
      "calendar.r".locations."/".proxyPass = "http://localhost:5232/";
    };
  };
  krebs.git = {
    enable = true;
    cgit.settings = {
      root-title = "krebs repos";
    };
    rules = with pkgs.stockholm.lib.git; [
      {
        user = [
          {
            name = "cal";
            pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGe1jtHaNFZKmWemWQVEGVYj+s4QGJaL9WYH+wokOZie";
          }
        ] ++ (lib.attrValues config.krebs.users);
        repo = [ config.krebs.git.repos.cal ];
        perm = push ''refs/heads/master'' [ create merge ];
      }
    ];
    repos.cal = {
      public = true;
      name = "cal";
      hooks = {
        post-receive = ''
          ${pkgs.git-hooks.irc-announce {
            channel = "#xxx";
            refs = [
              "refs/heads/master"
            ];
            nick = config.networking.hostName;
            server = "irc.r";
            verbose = true;
          }}
          /run/wrappers/bin/sudo -S -u radicale ${pushCal}
        '';
      };
    };
  };
  krebs.secret.files.calendar = {
    path = "/var/lib/radicale/.ssh/id_ed25519";
    owner = { name = "radicale"; };
    source-path = "${<secrets/radicale.id_ed25519>}";
  };

  security.sudo.extraConfig = ''
    git ALL=(radicale) NOPASSWD: ${pushCal}
  '';
}
