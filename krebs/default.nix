{ current-date
, current-host-name
, current-user-name
}@current: rec {

  deploy =
    { system ? current-host-name
    , target ? system
    }@args: let
      config = lib.get-config system;
    in ''
      #! /bin/sh
      # ${current-date} ${current-user-name}@${current-host-name}
      # krebs.deploy
      set -efu
      (${lib.populate args})
      ${lib.rootssh target ''
        ${lib.install args}
        ${config.krebs.build.profile}/bin/switch-to-configuration switch
      ''}
      echo OK
    '';

  infest =
    { system ? current-host-name
    , target ? system
    }@args: let
    in ''
      #! /bin/sh
      # ${current-date} ${current-user-name}@${current-host-name}
      # krebs.infest
      set -efu

      # XXX type -p is non-standard
      #export RSYNC_RSH; RSYNC_RSH="$(type -p ssh) \
      #  -o 'HostName $ {target.host.infest.addr}' \
      #  -o 'Port $ {toString target.host.infest.port}' \
      #"
      #ssh() {
      #  eval "$RSYNC_RSH \"\$@\""
      #}

      ${lib.rootssh target ''
        ${builtins.readFile ./4lib/infest/prepare.sh}
        ${builtins.readFile ./4lib/infest/install-nix.sh}
      ''}

      (${lib.populate args})

      ${lib.rootssh target ''
        export PATH; PATH=/root/.nix-profile/bin:$PATH

        src=$(type -p nixos-install)
        cat_src() {
          sed < "$src" "$(
            { sed < "$src" -n '
                  /^if ! test -e "\$mountPoint\/\$NIXOS_CONFIG/,/^fi$/=
                  /^nixpkgs=/=
                  /^NIX_PATH=/,/^$/{/./=}

                  # Disable: Copy the NixOS/Nixpkgs sources to the target as
                  # the initial contents of the NixOS channel.
                  /^srcs=/,/^ln -sfn /=
                '
            } | sed 's:$:s/^/#krebs#/:'
          )"
        }

        # Location to insert lib.install
        i=$(sed -n '/^echo "building the system configuration/=' "$src")

        {
          cat_src | sed -n "1,$i{p}"
          cat ${lib.doc (lib.install args)}
          cat_src | sed -n "$i,\''${$i!p}"
        } > nixos-install
        chmod +x nixos-install

        ## Wrap inserted lib.install into chroot.
        #nix_env=$(cat_src | sed -n '
        #  s:.*\(/nix/store/[a-z0-9]*-nix-[0-9.]\+/bin/nix-env\).*:\1:p;T;q
        #')
        #echo nix-env is $nix_env
        #sed -i '
        #  s:^nix-env:chroot $mountPoint '"$nix_env"':
        #' nixos-install

        ./nixos-install

        ${builtins.readFile ./4lib/infest/finalize.sh}
      ''}
    '';

  init =
    { system ? current-host-name
    }@args: let
      config = lib.get-config system;
    in ''
      #! /bin/sh
      # ${current-date} ${current-user-name}@${current-host-name}
      # krebs.init
      set -efu

      system=${lib.shell.escape system}
      secrets_dir=${config.krebs.build.source.dir.secrets.path}
      key_type=ed25519
      key_file=$secrets_dir/ssh.id_$key_type
      key_comment=$system

      if test -e "$key_file"; then
        echo "Warning: privkey already exists: $key_file" >&2
      else
        ssh-keygen \
            -C "$key_comment" \
            -t "$key_type" \
            -f "$key_file" \
            -N ""
        rm "$key_file.pub"
      fi

      pubkey=$(ssh-keygen -y -f "$key_file")

      cat<<EOF
      # put following into config.krebs.hosts.$system:
      ssh.pubkey = $(echo $pubkey | jq -R .);
      EOF
    '';

  lib = import ./4lib { lib = import <nixpkgs/lib>; } // rec {

    stockholm = import ../. current;

    get-config = system:
      stockholm.${current-user-name}.${system}.config
        or (abort "unknown system: ${system}");

    doc = s:
      let b = "EOF${builtins.hashString "sha256" s}"; in
      ''
      <<\${b}
      ${s}
      ${b}
      '';

    rootssh = target: script:
      "ssh root@${target} -T ${lib.doc ''
        set -efu
        ${script}
      ''}";

    install =
      { system ? current-host-name
      , target ? system
      }:
      let
        stockholm = import ../. {
          inherit current-date;
          inherit current-host-name;
          inherit current-user-name;
        };

        config = stockholm.${current-user-name}.${system}.config
          or (abort "unknown system: ${system}");

        nix-path =
          lib.concatStringsSep ":"
            (lib.mapAttrsToList (name: _: "${name}=/root/${name}")
              (config.krebs.build.source.dir //
               config.krebs.build.source.git));
      in ''
        set -efu
        NIX_PATH=${lib.shell.escape nix-path} \
        nix-env \
          --show-trace \
          -f '<stockholm>' \
          -Q \
          --argstr current-date ${lib.shell.escape current-date} \
          --argstr current-host-name ${lib.shell.escape current-host-name} \
          --argstr current-user-name ${lib.shell.escape current-user-name} \
          --profile ${lib.shell.escape config.krebs.build.profile} \
          --set \
          -A ${lib.escapeShellArg (lib.concatStringsSep "." [
                config.krebs.build.user.name
                config.krebs.build.host.name
                "system"
              ])}
      '';

    populate =
      { system ? current-host-name
      , target ? system
      }@args:
      let out = ''
          #! /bin/sh
          # ${current-date} ${current-user-name}@${current-host-name}
          set -efu
          ${lib.concatStringsSep "\n"
            (lib.concatMap
              (type: lib.mapAttrsToList (_: methods.${type})
                                        config.krebs.build.source.${type})
              ["dir" "git"])}
        '';

        stockholm = import ../. {
          inherit current-date;
          inherit current-host-name;
          inherit current-user-name;
        };

        config = stockholm.${current-user-name}.${system}.config
            or (abort "unknown system: ${system}");

        current-host = config.krebs.hosts.${current-host-name};
        current-user = config.krebs.users.${current-user-name};

        target-host = config.krebs.hosts.${system};

        methods.dir = config:
          let
            can-link = config.host.name == target-host.name;
            can-push = config.host.name == current-host.name;
            push-method = ''
              rsync \
                --exclude .git \
                --exclude .graveyard \
                --exclude old \
                --exclude tmp \
                --rsync-path='mkdir -p ${config.target-path} && rsync' \
                --delete-excluded \
                -vrLptgoD \
                ${config.path}/ \
                root@${target}:${config.target-path}
            '';
            url = "file://${config.host.name}${config.path}";
          in
          #if can-link then link-method else
          if can-push then push-method else
          throw "cannot source ${url}";

        methods.git = config:
          lib.rootssh target ''
            mkdir -p ${config.target-path}
            cd ${config.target-path}
            if ! test -e .git; then
              git init
            fi
            if ! cur_url=$(git config remote.origin.url 2>/dev/null); then
              git remote add origin ${config.url}
            elif test "$cur_url" != ${config.url}; then
              git remote set-url origin ${config.url}
            fi
            if test "$(git rev-parse --verify HEAD 2>/dev/null)" != ${config.rev}; then
              git fetch origin
              git checkout ${config.rev} -- .
              git checkout -q ${config.rev}
              git submodule init
              git submodule update
            fi
            git clean -dxf
          '';
      in out;
  };
}
