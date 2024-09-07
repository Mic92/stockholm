{ config, lib, pkgs, ... }:

# TODO unify logging of shell scripts to user and journal
# TODO move all scripts to ${etcDir}, so ControlMaster connections
#       immediately pick up new authenticators
# TODO when authorized_keys changes, then restart ssh
#       (or kill already connected users somehow)

with import ../../lib/pure.nix { inherit lib; };
let
  cfg = config.krebs.git;

  out = {
    options.krebs.git = api;
    config = with lib; lib.mkIf cfg.enable (mkMerge [
      (lib.mkIf cfg.cgit.enable cgit-imp)
      git-imp
    ]);
  };

  api = {
    enable = mkEnableOption "krebs.git";

    cgit = mkOption {
      type = types.submodule {
        options = {
          enable = mkEnableOption "krebs.git.cgit" // { default = true; };
          fcgiwrap = {
            group = mkOption {
              type = types.group;
              default = {
                name = "fcgiwrap";
              };
            };
            user = mkOption {
              type = types.user;
              default = {
                name = "fcgiwrap";
                home = toString pkgs.emptyDirectory;
              };
            };
          };
          settings = mkOption {
            apply = flip removeAttrs ["_module"];
            default = {};
            type = subtypes.cgit-settings;
          };
        };
      };
      default = {};
      description = ''
          Cgit is an attempt to create a fast web interface for the git version
          control system, using a built in cache to decrease pressure on the
          git server.
          cgit in this module is being served via fastcgi nginx.This module
          deploys a http://cgit.‹hostname› nginx configuration and enables nginx
          if not yet enabled.
          '';
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/git";
      description = "Directory used to store repositories.";
    };
    etcDir = mkOption {
      type = mkOptionType {
        name = "${types.absolute-pathname.name} starting with `/etc/'";
        check = x: types.absolute-pathname.check x && hasPrefix "/etc/" x;
        merge = mergeOneOption;
      };
      default = "/etc/git";
    };
    repos = mkOption {
      type = types.attrsOf subtypes.repo;
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
    rules = mkOption {
      type = types.listOf subtypes.rule;
      default = [];
      example = literalExample ''
        singleton {
          user = [ config.krebs.users.tv ];
          repo = [ testing ]; # see literal example of repos
          perm = push "refs/*" (with git; [
            non-fast-forward create delete merge
          ]);
        }
      '';
      description = ''
        access and permission rules for git repositories.
      '';
    };

    user = mkOption {
      type = types.user;
      default = {
        name = "git";
        home = toString pkgs.emptyDirectory;
      };
    };
  };

  # TODO put into krebs/4lib/types.nix?
  subtypes = {
    cgit-settings = types.submodule {
      # A setting's value of `null` means cgit's default should be used.
      options = {
        about-filter = mkOption {
          type = types.nullOr types.package;
          default = null;
        };
        cache-root = mkOption {
          type = types.absolute-pathname;
          default = "/tmp/cgit";
        };
        cache-size = mkOption {
          type = types.uint;
          default = 1000;
        };
        css = mkOption {
          type = types.absolute-pathname;
          default = "/static/cgit.css";
        };
        enable-commit-graph = mkOption {
          type = types.bool;
          default = true;
        };
        enable-index-links = mkOption {
          type = types.bool;
          default = true;
        };
        enable-index-owner = mkOption {
          type = types.bool;
          default = false;
        };
        enable-log-filecount = mkOption {
          type = types.bool;
          default = true;
        };
        enable-log-linecount = mkOption {
          type = types.bool;
          default = true;
        };
        enable-remote-branches = mkOption {
          type = types.bool;
          default = true;
        };
        logo = mkOption {
          type = types.absolute-pathname;
          default = "/static/cgit.png";
        };
        max-stats = mkOption {
          type =
            types.nullOr (types.enum ["week" "month" "quarter" "year"]);
          default = "year";
        };
        readme = mkOption {
          type = types.listOf types.str;
          default = [];
        };
        robots = mkOption {
          type = types.nullOr (types.listOf types.str);
          default = ["nofollow" "noindex"];
        };
        root-desc = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        root-title = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        source-filter = mkOption {
          type = types.nullOr types.absolute-pathname;
          default = null;
          example = literalExample
            "\${pkgs.cgit}/lib/cgit/filters/syntax-highlighting.py";
        };
        virtual-root = mkOption {
          type = types.nullOr types.absolute-pathname;
          default = "/";
        };
      };
    };
    repo = types.submodule ({ config, ... }: {
      options = {
        admins = mkOption {
          type = types.listOf types.user;
          default = [];
          description = ''
            List of users that should be able to do everything with this repo.

            This option is currently not used by krebs.git but instead can be
            used to create rules.  See e.g. ‹stockholm/lass/2configs/git.nix› for
            an example.
          '';
        };
        cgit = {
          desc = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Repository description.
            '';
          };
          path = mkOption {
            type = types.str;
            default = "${cfg.dataDir}/${config.name}";
            defaultText = "${cfg.dataDir}/‹reponame›";
            description = ''
              An absolute path to the repository directory. For non-bare
              repositories this is the .git-directory.
            '';
          };
          section = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Repository section.
            '';
          };
          url = mkOption {
            type = types.str;
            default = config.name;
            defaultText = "‹reponame›";
            description = ''
              The relative url used to access the repository.
            '';
          };
        };
        collaborators = mkOption {
          type = types.listOf types.user;
          default = [];
          description = ''
            List of users that should be able to fetch from this repo.

            This option is currently not used by krebs.git but instead can be
            used to create rules.  See e.g. ‹stockholm/tv/2configs/git.nix› for
            an example.
          '';
        };
        name = mkOption {
          type = types.str;
          description = ''
            Repository name.
          '';
          defaultText = "‹reponame›";
        };
        hooks = mkOption {
          type = types.attrsOf types.str;
          default = {};
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
    });
    rule = types.submodule ({ config, ... }: {
      options = {
        user = mkOption {
          type = types.listOf types.user;
          description = ''
            List of users this rule should apply to.
            Checked by authorize-command.
          '';
        };
        repo = mkOption {
          type = types.listOf subtypes.repo;
          description = ''
            List of repos this rule should apply to.
            Checked by authorize-command.
          '';
        };
        perm = mkOption {
          type = types.submodule {
            # TODO generate enum argument from krebs/4lib/git.nix
            options = {
              allow-commands = mkOption {
                type = types.listOf (types.enum (with git; [
                  git-receive-pack
                  git-upload-pack
                ]));
                default = [];
                description = ''
                  List of commands the rule's users are allowed to execute.
                  Checked by authorize-command.
                '';
              };
              allow-receive-ref = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = ''
                  Ref that can receive objects.
                  Checked by authorize-push.
                '';
              };
              allow-receive-modes = mkOption {
                type = types.listOf (types.enum (with git; [
                  fast-forward
                  non-fast-forward
                  create
                  delete
                  merge
                ]));
                default = [];
                description = ''
                  List of allowed receive modes.
                  Checked by pre-receive hook.
                '';
              };
            };
          };
          description = ''
            Permissions granted.
          '';
        };
      };
    });
  };

  git-imp = {
    system.activationScripts.git-init = "${init-script}";

    # TODO maybe put all scripts here and then use PATH?
    environment.etc.${removePrefix "/etc/" cfg.etcDir}.source =
      scriptFarm "git-ssh-authorizers" {
        authorize-command = makeAuthorizeScript (map (rule: [
          (map getName (toList rule.user))
          (map getName (toList rule.repo))
          (map getName rule.perm.allow-commands)
        ]) cfg.rules);

        authorize-push = makeAuthorizeScript (map (rule: [
          (map getName (toList rule.user))
          (map getName (toList rule.repo))
          (toList rule.perm.allow-receive-ref)
          (map getName rule.perm.allow-receive-modes)
        ]) (filter (rule: rule.perm.allow-receive-ref != null) cfg.rules));
      };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      description = "Git repository hosting user";
      # To allow running cgit-clear-cache via hooks.
      group = cfg.cgit.fcgiwrap.group.name;
      isSystemUser = true;
      shell = "/bin/sh";
      openssh.authorizedKeys.keys =
        unique
          (sort lessThan
                (map (makeAuthorizedKey git-ssh-command)
                     (filter (user: isString user.pubkey)
                             (concatMap (getAttr "user") cfg.rules))));
    };
    users.groups.${cfg.cgit.fcgiwrap.group.name} = {};
  };

  cgit-imp = {
    users = {
      groups.${cfg.cgit.fcgiwrap.group.name} = {
        inherit (cfg.cgit.fcgiwrap.group) name gid;
      };
      users.${cfg.cgit.fcgiwrap.user.name} = {
        inherit (cfg.cgit.fcgiwrap.user) home name uid;
        group = cfg.cgit.fcgiwrap.group.name;
        isSystemUser = true;
      };
    };

    services.fcgiwrap.instances.cgit = {
      enable = true;
      process.user = cfg.cgit.fcgiwrap.user.name;
      socket.user = cfg.cgit.fcgiwrap.user.name;
      process.group = cfg.cgit.fcgiwrap.group.name;
      socket.group = cfg.cgit.fcgiwrap.group.name;
      socket.address = "/run/fcgiwrap.sock";
      # socket.type = "unix" (default)
    };

    environment.etc."cgitrc".text = let
      repo-to-cgitrc = _: repo:
        optionals (isPublicRepo repo) (concatLists [
          [""] # empty line
          [(kv-to-cgitrc "repo.url" repo.cgit.url)]
          (mapAttrsToList kv-to-cgitrc
            (mapAttrs' (k: nameValuePair "repo.${k}")
              (removeAttrs repo.cgit ["url"])))
        ]);

      kv-to-cgitrc = k: v: getAttr (typeOf v) {
        bool = kv-to-cgitrc k (if v then 1 else 0);
        null = []; # This will be removed by `flatten`.
        list = {
          readme = map (x: "readme=${x}") v;
        }.${k} or "${k}=${concatStringsSep ", " v}";
        int = "${k}=${toString v}";
        set =
          if subtypes.cgit-settings.check v
            then "${k}=${v}"
            else error "kv-to-cgitrc: unhandled type: set";
        string = "${k}=${v}";
      };
    in
      concatStringsSep "\n"
        (flatten (
          mapAttrsToList kv-to-cgitrc cfg.cgit.settings
          ++
          mapAttrsToList repo-to-cgitrc cfg.repos
        ));

    environment.systemPackages = [
      (pkgs.cgit-clear-cache.override { inherit (cfg.cgit.settings) cache-root; })
    ];

    system.activationScripts.cgit = ''
      mkdir -m 0770 -p ${cfg.cgit.settings.cache-root}
      chmod 0770 ${cfg.cgit.settings.cache-root}
      chown ${toString cfg.cgit.fcgiwrap.user.name}:${toString cfg.cgit.fcgiwrap.group.name} ${cfg.cgit.settings.cache-root}
    '';

    services.nginx.virtualHosts.cgit = {
      serverAliases = [
        "cgit.${config.networking.hostName}"
        "cgit.${config.networking.hostName}.r"
      ];
      locations."/".extraConfig = ''
        include             ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param       SCRIPT_FILENAME ${pkgs.writers.writeDash "cgit-wrapper" ''
          set -efu
          exec 3>&1
          ${pkgs.cgit}/cgit/cgit.cgi "$@" 2>&1 >&3 3>&- \
            | ${pkgs.gnused}/bin/sed \
                  '
                    \|^${pkgs.cgit}/cgit/cgit.cgi: Relink |d
                  '
          exec 3>&-
        ''};
        fastcgi_param       PATH_INFO       $uri;
        fastcgi_param       QUERY_STRING    $args;
        fastcgi_param       HTTP_HOST       $server_name;
        fastcgi_pass        unix:${config.services.fcgiwrap.socketAddress};
      '';
      # Smart HTTP transport.  Regex based on.
      # https://github.com/git/git/blob/v2.27.0/http-backend.c#L708-L721
      locations."~ \"^/[0-9A-Za-z._-]+/(HEAD|info/refs|objects/info/(alternates|http-alternates|packs)|[0-9a-f]{2}/([0-9a-f]{38}|[0-9a-f]{62})|pack/pack-([0-9a-f]{40}|[0-9a-f]{64})\\.(pack|idx)|git-upload-pack|git-receive-pack)$\"".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param GIT_HTTP_EXPORT_ALL "";
        fastcgi_param GIT_PROJECT_ROOT ${cfg.dataDir};
        fastcgi_param HOME ${pkgs.write "git-http-backend.home" {
          "/.gitconfig".text = /* ini */ ''
            [safe]
            directory = .
            ${concatMapStrings
              (repo: "directory = ${cfg.dataDir}/${repo.name}\n")
              (attrValues cfg.repos)
            }
          '';
        }};
        fastcgi_param PATH_INFO $fastcgi_script_name;
        fastcgi_param SCRIPT_FILENAME ${pkgs.git}/bin/git-http-backend;
        fastcgi_pass unix:${config.services.fcgiwrap.socketAddress};
      '';
      locations."/static/".extraConfig = ''
        root ${pkgs.cgit}/cgit;
        rewrite ^/static(/.*)$ $1 break;
      '';
    };
  };

  getName = x: x.name;

  isPublicRepo = getAttr "public"; # TODO this is also in ./cgit.nix

  makeAuthorizedKey = git-ssh-command: user@{ name, pubkey, ... }:
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
      to-pattern = x: concatStringsSep "|" (toList x);
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

  # TODO use pkgs.write (from nix-writers)
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

    PATH=${makeBinPath (with pkgs; [
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

    PATH=${makeBinPath (with pkgs; [
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
          # TODO fix correctly with stringAfter
          chown -R ${toString config.users.users.git.uid}:nogroup "$repodir"
        fi
        ln -Tfs ${hooks} "$repodir/hooks"
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

      PATH=${makeBinPath (with pkgs; [
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

in
out
