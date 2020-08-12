{ config, pkgs, ... }:
with import <stockholm/lib>;

{
  krebs.gollum = {
    enable = true;
    extraConfig = ''
      Gollum::Hook.register(:post_commit, :hook_id) do |committer, sha1|
        system('${toString (pkgs.writers.writeDash "debuglol" ''
          export PATH=${makeBinPath [ pkgs.git ]}
          export GIT_SSH_COMMAND='${pkgs.openssh}/bin/ssh -i ${config.krebs.gollum.stateDir}/.ssh/id_ed25519'
          cd ${config.krebs.gollum.stateDir}
          if ! url=$(git config remote.origin.url); then
            git remote add origin git@localhost:gollum
          elif test "$url" != 'git@localhost:gollum'; then
            git remote set-url origin git@localhost:gollum
          fi
          git push origin master
        '')}')
      end
    '';
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    virtualHosts.wiki = {
      serverAliases = [ "wiki.r" "wiki.${config.networking.hostName}.r" ];
      locations."/".extraConfig = ''
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_pass http://127.0.0.1:${toString config.services.gollum.port};
      '';
    };
  };

  krebs.git = {
    enable = true;
    cgit.settings = {
      root-title = "krebs repos";
    };
    rules = with git; [
      {
        user = [
          {
            name = "gollum";
            pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMXbjDnQWg8EECsNRZZWezocMIiuENhCSQFcFUXcsOQ6";
          }
          config.krebs.users.lass-mors
        ];
        repo = [ config.krebs.git.repos.gollum ];
        perm = push ''refs/*'' [ create merge ];
      }
    ];
    repos.gollum = {
      public = true;
      name = "gollum";
      hooks = {
        post-receive = pkgs.git-hooks.irc-announce {
          channel = "#xxx";
          refs = [
            "refs/heads/master"
            "refs/heads/newest"
            "refs/tags/*"
          ];
          nick = config.networking.hostName;
          server = "irc.r";
          verbose = true;
        };
      };
    };
  };

  krebs.secret.files.gollum = {
    path = "${config.krebs.gollum.stateDir}/.ssh/id_ed25519";
    owner = { name = "gollum"; };
    source-path = "${<secrets/gollum.id_ed25519>}";
  };
}
