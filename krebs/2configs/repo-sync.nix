{ config, lib, pkgs, ... }:
with import ../../lib/pure.nix { inherit lib; };

let
  konsens-user = {
    name = "konsens";
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIKKozGNGBAzHnyj6xUlsjGxxknyChXvuyrddkWVVnz7";
  };
  mirror = "git@${config.networking.hostName}:";

  defineRepo = {
    name, desc, section
  }:
  let
    repo = {
      public = true;
      name = mkDefault "${name}";
      cgit.desc = desc;
      cgit.section = section;
      hooks = mkDefault {
        post-receive = pkgs.git-hooks.irc-announce {
          channel = "#xxx";
          refs = [
            "refs/heads/newest"
            "refs/tags/*"
          ];
          nick = config.networking.hostName;
          server = "irc.r";
          verbose = false;
        };
      };
    };
  in {
    rules = with git; [
      {
        user = with config.krebs.users; [
          config.krebs.users."${config.networking.hostName}-repo-sync"
          lass
          makefu
          tv
        ];
        repo = [ repo ];
        perm = push ''refs/*'' [ non-fast-forward create delete merge ];
      }
      {
        user = [
          konsens-user
        ];
        repo = [ repo ];
        perm = push "refs/heads/common" [ create merge ];
      }
      {
        user = attrValues config.krebs.users;
        repo = [ repo ];
        perm = fetch;
      }
    ];
    repos."${name}" = repo;
  };

  sync-repo = {
    name,
    remotes,
    desc ? "mirror for ${name}",
    section ? "mirror"
  }:
    {
      krebs.repo-sync.repos.${name} = {
        branches = (lib.mapAttrs' (user: url: lib.nameValuePair user {
          origin.url = url;
          mirror.url = "${mirror}${name}";
        }) remotes);
        latest = {
          url = "${mirror}${name}";
          ref = "heads/newest";
        };
      };
      krebs.git = defineRepo { inherit name desc section; };
    };

in {
  krebs.git = {
    enable = true;
    cgit.settings = {
      root-title = "krebs repos";
      root-desc = "keep calm and engage";
    };
  };
  krebs.repo-sync = {
    enable = true;
  };
  krebs.konsens = {
    enable = true;
    repos = {
      stockholm = {};
    };
  };
  krebs.secret.files.konsens = {
    path = "/var/lib/konsens/.ssh/id_ed25519";
    owner = konsens-user;
    source-path = "${<secrets/konsens.id_ed25519>}";
  };

  imports = [
    (sync-repo {
      name = "stockholm";
      desc = "take all computers hostage, they love it";
      section = "configuration";
      remotes = {
        makefu = "http://cgit.gum.r/stockholm";
        tv = "http://cgit.ni.r/stockholm";
        lassulus = "http://cgit.orange.r/stockholm";
      };
    })
    ({ krebs.git = defineRepo {
      name = "krops";
      desc = "deployment tools";
      section = "deployment";
    };})
  ];
}
