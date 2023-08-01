{ config, lib, pkgs, ... }: let
  acmeChallenge =
    { domain
    , nameserver
    , adminEmail
    , serial ? 0
    , refresh ? 3600
    , retry ? 900
    , expire ? 604800
    , minimum ? 180
    }:
    pkgs.writeText "${domain}.zone" /* bindzone */ ''
      $TTL 60
      @ IN SOA ${lib.concatStringsSep " " [
        "${nameserver}."
        "${lib.replaceStrings ["@"] ["."] adminEmail}."
        (toString serial)
        (toString refresh)
        (toString retry)
        (toString expire)
        (toString minimum)
      ]}
      @ IN NS ${nameserver}.
    '';
in {
  networking.firewall.allowedTCPPorts = [
    53 # domain for AXFR
  ];
  networking.firewall.allowedUDPPorts = [
    53 # domain
  ];

  krebs.systemd.services.knot.restartIfCredentialsChange = true;
  systemd.services.knot.serviceConfig.LoadCredential = [
    "keys.conf:/var/src/secrets/knot-keys.conf"
  ];

  services.knot = {
    enable = true;
    keyFiles = [
      "/run/credentials/knot.service/keys.conf"
    ];
    extraConfig = /* yaml */ ''
      server:
        udp-max-payload: 4096
        listen: [ 127.0.0.53@2, ${
          lib.concatMapStringsSep ", "
            (addr: "${addr}@53")
            (
              config.krebs.build.host.nets.internet.addrs or []
              ++
              # This is required for hosts at OCI because the default route
              # provided by DHCP is using the private address.
              config.krebs.build.host.nets.intranet.addrs or []
            )
        } ]

      log:
        - target: syslog
          any: debug

      remote:

      acl:
        - id: acme_acl
          key: acme
          action: update

        - id: dane_acl
          key: dane
          action: update

      mod-rrl:
        - id: default
          rate-limit: 200   # Allow 200 resp/s for each flow
          slip: 2           # Every other response slips

      policy:
        - id: rsa2k
          algorithm: rsasha256
          ksk-size: 4096
          zsk-size: 2048

      template:
        - id: default
          global-module: mod-rrl/default
          semantic-checks: on
          zonefile-sync: -1
          zonefile-load: difference-no-serial
          journal-content: all

      zone:
        - domain: krebsco.de
          file: ${pkgs.krebs.zones."krebsco.de"}
          dnssec-signing: on
          dnssec-policy: rsa2k
          acl: dane_acl

        - domain: _acme-challenge.krebsco.de
          file: ${acmeChallenge {
            domain = "_acme-challenge.krebsco.de";
            nameserver = "ns1.krebsco.de";
            adminEmail = "spam@krebsco.de";
          }}
          acl: acme_acl

        - domain: r
          file: ${pkgs.krebs.zones.r}

        - domain: w
          file: ${pkgs.krebs.zones.w}
    '';
  };

  systemd.services."knsupdate-krebsco.de" = {
    serviceConfig = {
      Type = "oneshot";
      SyslogIdentifier = "knsupdate-krebsco.de";
      ExecStart = pkgs.writeDash "knsupdate-krebsco.de" /* sh */ ''
        set -efu

        mk_certificate_association_data() {
          ${pkgs.openssl}/bin/openssl x509 -noout -fingerprint -sha256 < "$1" |
          ${pkgs.coreutils}/bin/cut -d= -f2 |
          ${pkgs.coreutils}/bin/tr -d :
        }

        certfile=/var/lib/acme/krebsco.de/cert.pem
        certificate_association_data=$(mk_certificate_association_data "$certfile")
        keyfile=/var/src/secrets/dane.tsig

        script=$(${pkgs.coreutils}/bin/mktemp -t knsupdate.XXXXXXXX)
        trap 'rm "$script"' EXIT
        (
          exec >"$script"
          echo server krebsco.de.
          echo zone krebsco.de.
          echo origin krebsco.de.
          echo add _25._tcp.ni 60 IN TLSA 3 0 1 $certificate_association_data
          echo add _443._tcp.ni 60 IN TLSA 3 0 1 $certificate_association_data
          echo show
          echo send
          echo answer
          echo quit
        )
        ${pkgs.knot-dns}/bin/knsupdate -k "$keyfile" "$script"
      '';
    };
  };
}
