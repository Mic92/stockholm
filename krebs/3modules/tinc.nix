with import <stockholm/lib>;
{ config, pkgs, ... }: {
  options.krebs.tinc = mkOption {
    default = {};
    description = ''
      define a tinc network
    '';
    type = types.attrsOf (types.submodule (tinc: {
      options = let
        netname = tinc.config._module.args.name;
      in {

        enable = mkEnableOption "krebs.tinc.${netname}" // { default = true; };

        confDir = mkOption {
          type = types.package;
          default = pkgs.linkFarm "${netname}-etc-tinc"
            (mapAttrsToList (name: path: { inherit name path; }) {
              "hosts" = tinc.config.hostsPackage;
              "tinc.conf" = pkgs.writeText "${netname}-tinc.conf" ''
                Name = ${tinc.config.host.name}
                Interface = ${netname}
                Broadcast = no
                ${concatMapStrings (c: "ConnectTo = ${c}\n") tinc.config.connectTo}
                Port = ${toString tinc.config.host.nets.${netname}.tinc.port}
                ${tinc.config.extraConfig}
              '';
              "tinc-up" = pkgs.writeDash "${netname}-tinc-up" ''
                ip link set ${netname} up
                ${tinc.config.tincUp}
              '';
            });
        };

        host = mkOption {
          type = types.host;
          default = config.krebs.build.host;
        };

        netname = mkOption {
          type = types.enum (attrNames tinc.config.host.nets);
          default = netname;
          description = ''
            The tinc network name.
            It is used to name the TUN device and to generate the default value for
            <literal>config.krebs.tinc.retiolum.hosts</literal>.
          '';
        };

        extraConfig = mkOption {
          type = types.lines;
          default = "";
          description = ''
            Extra Configuration to be appended to tinc.conf
          '';
        };
        tincUp = mkOption {
          type = types.str;
          default = let
            net = tinc.config.host.nets.${netname};
          in ''
            ${optionalString (net.ip4 != null) /* sh */ ''
              ip -4 addr add ${net.ip4.addr} dev ${netname}
              ip -4 route add ${net.ip4.prefix} dev ${netname}
            ''}
            ${optionalString (net.ip6 != null) /* sh */ ''
              ip -6 addr add ${net.ip6.addr} dev ${netname}
              ip -6 route add ${net.ip6.prefix} dev ${netname}
            ''}
            ${tinc.config.tincUpExtra}
          '';
          defaultText = ''
            ip -4 addr add ‹net.ip4.addr› dev ${netname}
            ip -4 route add ‹net.ip4.prefix› dev ${netname}
            ip -6 addr add ‹net.ip6.addr› dev ${netname}
            ip -6 route add ‹net.ip6.prefix› dev ${netname}
            ${tinc.config.tincUpExtra}
          '';
          description = ''
            tinc-up script to be used. Defaults to setting the
            krebs.host.nets.‹netname›.ip4 and ip6 for the new ips and
            configures forwarding of the respecitive netmask as subnet.
          '';
        };

        tincUpExtra = mkOption {
          type = types.str;
          default = "";
        };

        tincPackage = mkOption {
          type = types.package;
          default = pkgs.tinc_pre;
          description = "Tincd package to use.";
        };

        hosts = mkOption {
          type = with types; attrsOf host;
          default =
            filterAttrs (_: h: hasAttr tinc.config.netname h.nets) config.krebs.hosts;
          defaultText = "‹all-hosts-of-‹netname››";
          description = ''
            Hosts to generate <literal>config.krebs.tinc.retiolum.hostsPackage</literal>.
            Note that these hosts must have a network named
            <literal>config.krebs.tinc.retiolum.netname</literal>.
          '';
        };

        hostsArchive = mkOption {
          type = types.package;
          default = pkgs.runCommand "retiolum-hosts.tar.bz2" {
            nativeBuildInputs = [ pkgs.gnutar pkgs.coreutils ];
          } ''
            cp \
                --no-preserve=mode \
                --recursive \
                ${tinc.config.hostsPackage} \
                hosts
            tar -cjf $out hosts
          '';
          readOnly = true;
        };

        hostsPackage = mkOption {
          type = types.package;
          default = pkgs.stdenv.mkDerivation {
            name = "${tinc.config.netname}-tinc-hosts";
            phases = [ "installPhase" ];
            installPhase = ''
              mkdir $out
              ${concatStrings (mapAttrsToList (_: host: ''
                echo ${shell.escape host.nets."${tinc.config.netname}".tinc.config} \
                  > $out/${shell.escape host.name}
              '') tinc.config.hosts)}
            '';
          };
          defaultText = "‹netname›-tinc-hosts";
          description = ''
            Package of tinc host configuration files.  By default, a package will
            be generated from <literal>config.krebs.‹netname›.hosts</literal>.  This
            option's main purpose is to expose the generated hosts package to other
            modules, like <literal>config.krebs.tinc_graphs</literal>.  But it can
            also be used to provide a custom hosts directory.
          '';
          example = literalExample ''
            (pkgs.stdenv.mkDerivation {
              name = "my-tinc-hosts";
              src = /home/tv/my-tinc-hosts;
              installPhase = "cp -R . $out";
            })
          '';
        };

        iproutePackage = mkOption {
          type = types.package;
          default = pkgs.iproute;
          description = "Iproute2 package to use.";
        };

        privkey = mkOption {
          type = types.absolute-pathname;
          default = toString <secrets> + "/${tinc.config.netname}.rsa_key.priv";
          defaultText = "‹secrets/‹netname›.rsa_key.priv›";
        };

        privkey_ed25519 = mkOption {
          type = types.nullOr types.absolute-pathname;
          default =
            if tinc.config.host.nets.${netname}.tinc.pubkey_ed25519 == null
              then null
              else toString <secrets> + "/${tinc.config.netname}.ed25519_key.priv";
          defaultText = "‹secrets/‹netname›.ed25519_key.priv›";
        };

        connectTo = mkOption {
          type = types.listOf types.str;
          ${if netname == "retiolum" then "default" else null} = [
            "gum"
            "ni"
            "prism"
          ];
          description = ''
            The list of hosts in the network which the client will try to connect
            to.  These hosts should have an 'Address' configured which points to a
            routeable IPv4 or IPv6 address.

            In stockholm this can be done by configuring:
            {
              krebs.hosts.‹host›.nets.‹netname›.via.ip4.addr = external-ip;
              krebs.hosts.‹host›.nets.‹netname›.tinc.port = 1655;
            }
          '';
        };

        user = mkOption {
          type = types.user;
          default = {
            name = tinc.config.netname;
            home = "/var/lib/${tinc.config.user.name}";
          };
          defaultText = {
            name = "‹netname›";
            home = "/var/lib/‹netname›";
          };
        };
      };
    }));
  };

  config = {
    users.users = mapAttrs' (netname: cfg:
      nameValuePair "${netname}" {
        inherit (cfg.user) home name uid;
        createHome = true;
        isSystemUser = true;
        group = netname;
      }
    ) config.krebs.tinc;

    users.groups = mapAttrs' (netname: cfg:
      nameValuePair netname {}
    ) config.krebs.tinc;

    krebs.systemd.services = mapAttrs (netname: cfg: {
    }) config.krebs.tinc;

    systemd.services = mapAttrs (netname: cfg: {
      description = "Tinc daemon for ${netname}";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [
        cfg.iproutePackage
        cfg.tincPackage
      ];
      reloadIfChanged = true;
      serviceConfig = {
        Restart = "always";
        LoadCredential = filter (x: x != "") [
          (optionalString (cfg.privkey_ed25519 != null)
            "ed25519_key:${cfg.privkey_ed25519}"
          )
          "rsa_key:${cfg.privkey}"
        ];
        ExecStartPre = pkgs.writers.writeDash "init-tinc-${netname}" ''
          ${pkgs.coreutils}/bin/mkdir -p /etc/tinc
          ${pkgs.rsync}/bin/rsync -vaL --delete \
            --chown ${cfg.user.name} \
            --chmod u=rwX,g=rX \
            ${cfg.confDir}/ /etc/tinc/${netname}/
        '';
        ExecStart = toString [
          "${cfg.tincPackage}/sbin/tincd"
          "-D"
          "-U ${cfg.user.name}"
          "-c /etc/tinc/${netname}"
          "-d 0"
          (optionalString (cfg.privkey_ed25519 != null)
            "-o Ed25519PrivateKeyFile=\${CREDENTIALS_DIRECTORY}/ed25519_key"
          )
          "-o PrivateKeyFile=\${CREDENTIALS_DIRECTORY}/rsa_key"
          "--pidfile=/var/run/tinc.${netname}.pid"
        ];
        ExecReload = "${cfg.tincPackage}/sbin/tinc -n ${netname} restart";
        SyslogIdentifier = netname;
      };
    }) config.krebs.tinc;
  };
}
