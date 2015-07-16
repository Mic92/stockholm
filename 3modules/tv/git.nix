{ config, pkgs, lib, ... }:

# TODO unify logging of shell scripts to user and journal
# TODO move all scripts to ${etcDir}, so ControlMaster connections
#       immediately pick up new authenticators
# TODO when authorized_keys changes, then restart ssh
#       (or kill already connected users somehow)

with builtins;
with lib;
let
  cfg = config.tv.git;

  out = {
    imports = [
      ../../3modules/tv/nginx.nix
    ];
    options.tv.git = api;
    config = mkIf cfg.enable (mkMerge [
      (mkIf cfg.cgit cgit-imp)
      git-imp
    ]);
  };

  api = {
    enable = mkEnableOption "tv.git";

    cgit = mkOption {
      type = types.bool;
      default = true;
      description = "Enable cgit."; # TODO better desc; talk about nginx
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/git";
      description = "Directory used to store repositories.";
    };
    etcDir = mkOption {
      type = types.str;
      default = "/etc/git";
    };
    repos = mkOption {
      type = types.attrsOf (types.submodule ({
        options = {
          desc = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Repository description.
            '';
          };
          section = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Repository section.
            '';
          };
          name = mkOption {
            type = types.str;
            description = ''
              Repository name.
            '';
          };
          hooks = mkOption {
            type = types.attrsOf types.str;
            description = ''
              Repository-specific hooks.
            '';
          };
          public = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Allow everybody to read the repository via HTTP if cgit enabled.
            '';
            # TODO allow every configured user to fetch the repository via SSH.
          };
        };
      }));

      default = {};

      example = literalExample ''
        {
          testing = {
            name = "testing";
            hooks.post-update = '''
              #! /bin/sh
              set -euf
              echo post-update hook: $* >&2
            ''';
          };
          testing2 = { name = "testing2"; };
        }
      '';

      description = ''
        Repositories.
      '';
    };
    root-desc = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Text printed below the heading on the repository index page.
        Default value: "a fast webinterface for the git dscm".
      '';
    };
    root-title = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Text printed as heading on the repository index page.
        Default value: "Git Repository Browser".
      '';
    };
    rules = mkOption {
      type = types.unspecified;
    };
    users = mkOption {
      type = types.unspecified;
    };
  };

  git-imp = {
    system.activationScripts.git-init = "${init-script}";
    
    # TODO maybe put all scripts here and then use PATH?
    environment.etc."${etc-base}".source =
      scriptFarm "git-ssh-authorizers" {
        authorize-command = makeAuthorizeScript (map ({ repo, user, perm }: [
          (map getName (ensureList user))
          (map getName (ensureList repo))
          (map getName perm.allow-commands)
        ]) cfg.rules);
    
        authorize-push = makeAuthorizeScript (map ({ repo, user, perm }: [
          (map getName (ensureList user))
          (map getName (ensureList repo))
          (ensureList perm.allow-receive-ref)
          (map getName perm.allow-receive-modes)
        ]) (filter (x: hasAttr "allow-receive-ref" x.perm) cfg.rules));
      };
    
    users.extraUsers = singleton {
      description = "Git repository hosting user";
      name = "git";
      shell = "/bin/sh";
      openssh.authorizedKeys.keys =
        mapAttrsToList (_: makeAuthorizedKey git-ssh-command) cfg.users;
      uid = 112606723; # genid git
    };
  };

  cgit-imp = {
    users.extraUsers = lib.singleton {
      inherit (fcgitwrap-user) group name uid;
      home = toString (pkgs.runCommand "empty" {} "mkdir -p $out");
    };

    users.extraGroups = lib.singleton {
      inherit (fcgitwrap-group) gid name;
    };

    services.fcgiwrap = {
      enable = true;
      user = fcgitwrap-user.name;
      group = fcgitwrap-user.group;
      # socketAddress = "/run/fcgiwrap.sock" (default)
      # socketType = "unix" (default)
    };

    environment.etc."cgitrc".text = ''
      css=/static/cgit.css
      logo=/static/cgit.png

      # if you do not want that webcrawler (like google) index your site
      robots=noindex, nofollow

      virtual-root=/

      # TODO make this nicer (and/or somewhere else)
      cache-root=/tmp/cgit

      cache-size=1000
      enable-commit-graph=1
      enable-index-links=1
      enable-index-owner=0
      enable-log-filecount=1
      enable-log-linecount=1
      enable-remote-branches=1

      ${optionalString (cfg.root-title != null) "root-title=${cfg.root-title}"}
      ${optionalString (cfg.root-desc != null) "root-desc=${cfg.root-desc}"}

      snapshots=0
      max-stats=year

      ${concatMapStringsSep "\n" (repo: ''
        repo.url=${repo.name}
        repo.path=${cfg.dataDir}/${repo.name}
        ${optionalString (repo.section != null) "repo.section=${repo.section}"}
        ${optionalString (repo.desc != null) "repo.desc=${repo.desc}"}
      '') (filter isPublicRepo (attrValues cfg.repos))}
    '';

    system.activationScripts.cgit = ''
      mkdir -m 0700 -p /tmp/cgit
      chown ${toString fcgitwrap-user.uid}:${toString fcgitwrap-group.gid} /tmp/cgit
    '';

    tv.nginx = {
      enable = true;
      servers.cgit = {
        server-names = [
          "cgit.${config.networking.hostName}"
          "cgit.${config.networking.hostName}.retiolum"
        ];
        locations = [
          (nameValuePair "/" ''
            include             ${pkgs.nginx}/conf/fastcgi_params;
            fastcgi_param       SCRIPT_FILENAME ${pkgs.cgit}/cgit/cgit.cgi;
            fastcgi_param       PATH_INFO       $uri;
            fastcgi_param       QUERY_STRING    $args;
            fastcgi_param       HTTP_HOST       $server_name;
            fastcgi_pass        unix:${config.services.fcgiwrap.socketAddress};
          '')
          (nameValuePair "/static/" ''
            root ${pkgs.cgit}/cgit;
            rewrite ^/static(/.*)$ $1 break;
          '')
        ];
      };
    };
  };

  fcgitwrap-user = {
    name = "fcgiwrap";
    uid = 2851179180; # genid fcgiwrap
    group = "fcgiwrap";
  };

  fcgitwrap-group = {
    name = "fcgiwrap";
    gid = 2851179180; # genid fcgiwrap
  };


  ensureList = x:
    if typeOf x == "list" then x else [x];

  getName = x: x.name;

  isPublicRepo = getAttr "public"; # TODO this is also in ./cgit.nix

  makeAuthorizedKey = git-ssh-command: user@{ name, pubkey }:
    # TODO assert name
    # TODO assert pubkey
    let
      options = concatStringsSep "," [
        ''command="exec ${git-ssh-command} ${name}"''
        "no-agent-forwarding"
        "no-port-forwarding"
        "no-pty"
        "no-X11-forwarding"
      ];
    in
    "${options} ${pubkey}";

  # [case-pattern] -> shell-script
  # Create a shell script that succeeds (exit 0) when all its arguments
  # match the case patterns (in the given order).
  makeAuthorizeScript =
    let
      # TODO escape
      to-pattern = x: concatStringsSep "|" (ensureList x);
      go = i: ps:
        if ps == []
          then "exit 0"
          else ''
            case ''$${toString i} in ${to-pattern (head ps)})
            ${go (i + 1) (tail ps)}
            esac'';
    in
    patterns: ''
      #! /bin/sh
      set -euf
      ${concatStringsSep "\n" (map (go 1) patterns)}
      exit -1
    '';

  reponames = rules: sort lessThan (unique (map (x: x.repo.name) rules));

  # TODO makeGitHooks that uses runCommand instead of scriptFarm?
  scriptFarm =
    farm-name: scripts:
    let
      makeScript = script-name: script-string: {
        name = script-name;
        path = pkgs.writeScript "${farm-name}_${script-name}" script-string;
      };
    in
    pkgs.linkFarm farm-name (mapAttrsToList makeScript scripts);


  git-ssh-command = pkgs.writeScript "git-ssh-command" ''
    #! /bin/sh
    set -euf

    PATH=${makeSearchPath "bin" (with pkgs; [
      coreutils
      git
      gnugrep
      gnused
      systemd
    ])}

    abort() {
      echo "error: $1" >&2
      systemd-cat -p err -t git echo "error: $1"
      exit -1
    }

    GIT_SSH_USER=$1

    systemd-cat -p info -t git echo \
      "authorizing $GIT_SSH_USER $SSH_CONNECTION $SSH_ORIGINAL_COMMAND"

    # References: The Base Definitions volume of
    # POSIX.1‐2013, Section 3.278, Portable Filename Character Set
    portable_filename_bre="^[A-Za-z0-9._-]\\+$"

    command=$(echo "$SSH_ORIGINAL_COMMAND" \
      | sed -n 's/^\([^ ]*\) '"'"'\(.*\)'"'"'/\1/p' \
      | grep "$portable_filename_bre" \
      || abort 'cannot read command')

    GIT_SSH_REPO=$(echo "$SSH_ORIGINAL_COMMAND" \
      | sed -n 's/^\([^ ]*\) '"'"'\(.*\)'"'"'/\2/p' \
      | grep "$portable_filename_bre" \
      || abort 'cannot read reponame')

    ${cfg.etcDir}/authorize-command \
        "$GIT_SSH_USER" "$GIT_SSH_REPO" "$command" \
      || abort 'access denied'

    repodir=${escapeShellArg cfg.dataDir}/$GIT_SSH_REPO

    systemd-cat -p info -t git \
      echo "authorized exec $command $repodir"

    export GIT_SSH_USER
    export GIT_SSH_REPO
    exec "$command" "$repodir"
  '';

  init-script = pkgs.writeScript "git-init" ''
    #! /bin/sh
    set -euf

    PATH=${makeSearchPath "bin" (with pkgs; [
      coreutils
      findutils
      gawk
      git
      gnugrep
      gnused
    ])}

    dataDir=${escapeShellArg cfg.dataDir}
    mkdir -p "$dataDir"

    # Notice how the presence of hooks symlinks determine whether
    # we manage a repositry or not.

    # Make sure that no existing repository has hooks.  We can delete
    # symlinks because we assume we created them.
    find "$dataDir" -mindepth 2 -maxdepth 2 -name hooks -type l -delete
    bad_hooks=$(find "$dataDir" -mindepth 2 -maxdepth 2 -name hooks)
    if echo "$bad_hooks" | grep -q .; then
      printf 'error: unknown hooks:\n%s\n' \
        "$(echo "$bad_hooks" | sed 's/^/  /')" \
        >&2
      exit -1
    fi

    # Initialize repositories.
    ${concatMapStringsSep "\n" (repo:
      let
        hooks = scriptFarm "git-hooks" (makeHooks repo);
      in
      ''
        reponame=${escapeShellArg repo.name}
        repodir=$dataDir/$reponame
        mode=${toString (if isPublicRepo repo then 0711 else 0700)}
        if ! test -d "$repodir"; then
          mkdir -m "$mode" "$repodir"
          git init --bare --template=/var/empty "$repodir"
          chown -R git:nogroup "$repodir"
        fi
        ln -s ${hooks} "$repodir/hooks"
      ''
    ) (attrValues cfg.repos)}

    # Warn about repositories that exist but aren't mentioned in the
    # current configuration (and thus didn't receive a hooks symlink).
    unknown_repos=$(find "$dataDir" -mindepth 1 -maxdepth 1 \
      -type d \! -exec test -e '{}/hooks' \; -print)
    if echo "$unknown_repos" | grep -q .; then
      printf 'warning: stale repositories:\n%s\n' \
        "$(echo "$unknown_repos" | sed 's/^/  /')" \
        >&2
    fi
  '';

  makeHooks = repo: removeAttrs repo.hooks [ "pre-receive" ] // {
    pre-receive = ''
      #! /bin/sh
      set -euf

      PATH=${makeSearchPath "bin" (with pkgs; [
        coreutils # env
        git
        systemd
      ])}

      accept() {
        #systemd-cat -p info -t git echo "authorized $1"
        accept_string="''${accept_string+$accept_string
      }authorized $1"
      }
      reject() {
        #systemd-cat -p err -t git echo "denied $1"
        #echo 'access denied' >&2
        #exit_code=-1
        reject_string="''${reject_string+$reject_string
      }access denied: $1"
      }

      empty=0000000000000000000000000000000000000000

      accept_string=
      reject_string=
      while read oldrev newrev ref; do

        if [ $oldrev = $empty ]; then
          receive_mode=create
        elif [ $newrev = $empty ]; then
          receive_mode=delete
        elif [ "$(git merge-base $oldrev $newrev)" = $oldrev ]; then
          receive_mode=fast-forward
        else
          receive_mode=non-fast-forward
        fi

        if ${cfg.etcDir}/authorize-push \
            "$GIT_SSH_USER" "$GIT_SSH_REPO" "$ref" "$receive_mode"; then
          accept "$receive_mode $ref"
        else
          reject "$receive_mode $ref"
        fi
      done

      if [ -n "$reject_string" ]; then
        systemd-cat -p err -t git echo "$reject_string"
        exit -1
      fi

      systemd-cat -p info -t git echo "$accept_string"

      ${optionalString (hasAttr "post-receive" repo.hooks) ''
        # custom post-receive hook
        ${repo.hooks.post-receive}''}
    '';
  };

  etc-base =
    assert (hasPrefix "/etc/" cfg.etcDir);
    removePrefix "/etc/" cfg.etcDir;

in
out
