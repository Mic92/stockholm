{ config, lib, pkgs, ... }: let
  vpnIp = "85.202.81.161";
in {
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
  ];

  krebs.build.host = config.krebs.hosts.yellow;

  lass.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN737BAP36KiZO97mPKTIUGJUcr97ps8zjfFag6cUiYL";
  };

  users.groups.download.members = [ "transmission" ];

  networking.useHostResolvConf = false;
  networking.useNetworkd = true;
  services.transmission = {
    enable = true;
    home = "/var/state/transmission";
    group = "download";
    downloadDirPermissions = "775";
    settings = {
      download-dir = "/var/download/transmission";
      incomplete-dir-enabled = false;
      rpc-bind-address = "::";
      message-level = 1;
      umask = 18;
      rpc-whitelist-enabled = false;
      rpc-host-whitelist-enabled = false;
    };
  };

  services.nginx = {
    enable = true;
    package = pkgs.nginx.override {
      modules = with pkgs.nginxModules; [
        fancyindex
      ];
    };
    virtualHosts.default = {
      default = true;
      locations."/" = {
        root = "/var/download";
        extraConfig = ''
          fancyindex on;
          fancyindex_footer "/fancy.html";
          include ${pkgs.nginx}/conf/mime.types;
          include ${pkgs.writeText "extrMime" ''
            types {
              video/webm mkv;
            }
          ''};
          create_full_put_path on;
        '';
      };
      locations."/chatty" = {
        proxyPass = "http://localhost:3000";
        extraConfig = ''
          rewrite /chatty/(.*) /$1  break;
          proxy_set_header Host $host;
        '';
      };
      locations."= /fancy.html".extraConfig = ''
        alias ${pkgs.writeText "nginx_footer" ''
          <div id="mydiv">
            <!-- Include a header DIV with the same name as the draggable DIV, followed by "header" -->
            <div id="mydivheader">Click here to move</div>
              <iframe src="/chatty/index.html"></iframe>
          </div>
          <style>
          #mydiv {
            position: absolute;
            z-index: 9;
            background-color: #f1f1f1;
            border: 1px solid #d3d3d3;
            text-align: center;
          }

          #mydivheader {
            padding: 10px;
            cursor: move;
            z-index: 10;
            background-color: #2196F3;
            color: #fff;
          }
          </style>
          <script>
            // Make the DIV element draggable:
            dragElement(document.getElementById("mydiv"));

            function dragElement(elmnt) {
              var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
              if (document.getElementById(elmnt.id + "header")) {
                // if present, the header is where you move the DIV from:
                document.getElementById(elmnt.id + "header").onmousedown = dragMouseDown;
              } else {
                // otherwise, move the DIV from anywhere inside the DIV:
                elmnt.onmousedown = dragMouseDown;
              }

              function dragMouseDown(e) {
                e = e || window.event;
                e.preventDefault();
                // get the mouse cursor position at startup:
                pos3 = e.clientX;
                pos4 = e.clientY;
                document.onmouseup = closeDragElement;
                // call a function whenever the cursor moves:
                document.onmousemove = elementDrag;
              }

              function elementDrag(e) {
                e = e || window.event;
                e.preventDefault();
                // calculate the new cursor position:
                pos1 = pos3 - e.clientX;
                pos2 = pos4 - e.clientY;
                pos3 = e.clientX;
                pos4 = e.clientY;
                // set the element's new position:
                elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
                elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
              }

              function closeDragElement() {
                // stop moving when mouse button is released:
                document.onmouseup = null;
                document.onmousemove = null;
              }
            }
          </script>
        ''};
      '';
    };
    virtualHosts."jelly.r" = {
      locations."/".extraConfig = ''
        proxy_pass http://localhost:8096/;
        proxy_set_header Accept-Encoding "";
      '';
    };
    virtualHosts."rada.r" = {
      locations."/" = {
        proxyPass = "http://localhost:7878";
      };
    };
    virtualHosts."sona.r" = {
      locations."/" = {
        proxyPass = "http://localhost:8989";
      };
    };
  };

  services.samba = {
    enable = true;
    enableNmbd = false;
    extraConfig = ''
      workgroup = WORKGROUP
      server string = ${config.networking.hostName}
      # only allow retiolum addresses
      hosts allow = 42::/16 10.243.0.0/16 10.244.0.0/16

      # Use sendfile() for performance gain
      use sendfile = true

      # No NetBIOS is needed
      disable netbios = true

      # Only mangle non-valid NTFS names, don't care about DOS support
      mangled names = illegal

      # Performance optimizations
      socket options = TCP_NODELAY IPTOS_LOWDELAY SO_RCVBUF=65536 SO_SNDBUF=65536

      # Disable all printing
      load printers = false
      disable spoolss = true
      printcap name = /dev/null

      map to guest = Bad User
      max log size = 50
      dns proxy = no
      security = user

      [global]
      syslog only = yes
    '';
    shares.public = {
      comment = "Warez";
      path = "/var/download";
      public = "yes";
      "only guest" = "yes";
      "create mask" = "0644";
      "directory mask" = "2777";
      writable = "no";
      printable = "no";
    };
  };

  systemd.services.bruellwuerfel =
  let
    bruellwuerfelSrc = pkgs.fetchFromGitHub {
      owner = "krebs";
      repo = "bruellwuerfel";
      rev = "dc73adf69249fb63a4b024f1f3fbc9e541b27015";
      sha256 = "078jp1gbavdp8lnwa09xa5m6bbbd05fi4x5ldkkgin5z04hwlhmd";
    };
  in {
    wantedBy = [ "multi-user.target" ];
    environment = {
      IRC_CHANNEL = "#flix";
      IRC_NICK = "bruelli";
      IRC_SERVER = "irc.r";
      IRC_HISTORY_FILE = "/tmp/bruelli.history";
    };
    serviceConfig = {
      ExecStart = "${pkgs.deno}/bin/deno run -A ${bruellwuerfelSrc}/src/index.ts";
    };
  };

  krebs.iptables = {
    enable = true;
    tables.filter.INPUT.rules = [
      { predicate = "-p tcp --dport 80"; target = "ACCEPT"; } # nginx web dir
      { predicate = "-p tcp --dport 9091"; target = "ACCEPT"; } # transmission-web
      { predicate = "-p tcp --dport 51413"; target = "ACCEPT"; } # transmission-traffic
      { predicate = "-p udp --dport 51413"; target = "ACCEPT"; } # transmission-traffic
      { predicate = "-p tcp --dport 8096"; target = "ACCEPT"; } # jellyfin
      { predicate = "-p tcp --dport 9696"; target = "ACCEPT"; } # prowlarr
      { predicate = "-p tcp --dport 8989"; target = "ACCEPT"; } # sonarr
      { predicate = "-p tcp --dport 7878"; target = "ACCEPT"; } # radarr

      # smbd
      { predicate = "-i retiolum -p tcp --dport 445"; target = "ACCEPT"; }
      { predicate = "-i retiolum -p tcp --dport 111"; target = "ACCEPT"; }
      { predicate = "-i retiolum -p udp --dport 111"; target = "ACCEPT"; }
      { predicate = "-i retiolum -p tcp --dport 2049"; target = "ACCEPT"; }
      { predicate = "-i retiolum -p udp --dport 2049"; target = "ACCEPT"; }
      { predicate = "-i retiolum -p tcp --dport 4000:4002"; target = "ACCEPT"; }
      { predicate = "-i retiolum -p udp --dport 4000:4002"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p tcp --dport 445"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p tcp --dport 111"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p udp --dport 111"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p tcp --dport 2049"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p udp --dport 2049"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p tcp --dport 4000:4002"; target = "ACCEPT"; }
      { predicate = "-i wiregrill -p udp --dport 4000:4002"; target = "ACCEPT"; }
    ];
    tables.filter.OUTPUT = {
      policy = "DROP";
      rules = [
        { predicate = "-o lo"; target = "ACCEPT"; }
        { v6 = false; predicate = "-d ${vpnIp}/32"; target = "ACCEPT"; }
        { predicate = "-o tun0"; target = "ACCEPT"; }
        { predicate = "-o retiolum"; target = "ACCEPT"; }
        { v6 = false; predicate = "-d 1.1.1.1/32"; target = "ACCEPT"; }
        { v6 = false; predicate = "-d 1.0.0.1/32"; target = "ACCEPT"; }
        { v6 = false; predicate = "-o eth0 -d 10.233.2.0/24"; target = "ACCEPT"; }
      ];
    };
  };

  services.openvpn.servers.nordvpn.config = ''
    client
    dev tun
    proto udp
    remote ${vpnIp} 1194
    resolv-retry infinite
    remote-random
    nobind
    tun-mtu 1500
    tun-mtu-extra 32
    mssfix 1450
    persist-key
    persist-tun
    ping 15
    ping-restart 15
    ping-timer-rem
    reneg-sec 0
    comp-lzo no

    remote-cert-tls server

    auth-user-pass ${toString <secrets/nordvpn.txt>}
    verb 3
    pull
    fast-io
    cipher AES-256-CBC
    auth SHA512

    <ca>
    -----BEGIN CERTIFICATE-----
    MIIFCjCCAvKgAwIBAgIBATANBgkqhkiG9w0BAQ0FADA5MQswCQYDVQQGEwJQQTEQ
    MA4GA1UEChMHTm9yZFZQTjEYMBYGA1UEAxMPTm9yZFZQTiBSb290IENBMB4XDTE2
    MDEwMTAwMDAwMFoXDTM1MTIzMTIzNTk1OVowOTELMAkGA1UEBhMCUEExEDAOBgNV
    BAoTB05vcmRWUE4xGDAWBgNVBAMTD05vcmRWUE4gUm9vdCBDQTCCAiIwDQYJKoZI
    hvcNAQEBBQADggIPADCCAgoCggIBAMkr/BYhyo0F2upsIMXwC6QvkZps3NN2/eQF
    kfQIS1gql0aejsKsEnmY0Kaon8uZCTXPsRH1gQNgg5D2gixdd1mJUvV3dE3y9FJr
    XMoDkXdCGBodvKJyU6lcfEVF6/UxHcbBguZK9UtRHS9eJYm3rpL/5huQMCppX7kU
    eQ8dpCwd3iKITqwd1ZudDqsWaU0vqzC2H55IyaZ/5/TnCk31Q1UP6BksbbuRcwOV
    skEDsm6YoWDnn/IIzGOYnFJRzQH5jTz3j1QBvRIuQuBuvUkfhx1FEwhwZigrcxXu
    MP+QgM54kezgziJUaZcOM2zF3lvrwMvXDMfNeIoJABv9ljw969xQ8czQCU5lMVmA
    37ltv5Ec9U5hZuwk/9QO1Z+d/r6Jx0mlurS8gnCAKJgwa3kyZw6e4FZ8mYL4vpRR
    hPdvRTWCMJkeB4yBHyhxUmTRgJHm6YR3D6hcFAc9cQcTEl/I60tMdz33G6m0O42s
    Qt/+AR3YCY/RusWVBJB/qNS94EtNtj8iaebCQW1jHAhvGmFILVR9lzD0EzWKHkvy
    WEjmUVRgCDd6Ne3eFRNS73gdv/C3l5boYySeu4exkEYVxVRn8DhCxs0MnkMHWFK6
    MyzXCCn+JnWFDYPfDKHvpff/kLDobtPBf+Lbch5wQy9quY27xaj0XwLyjOltpiST
    LWae/Q4vAgMBAAGjHTAbMAwGA1UdEwQFMAMBAf8wCwYDVR0PBAQDAgEGMA0GCSqG
    SIb3DQEBDQUAA4ICAQC9fUL2sZPxIN2mD32VeNySTgZlCEdVmlq471o/bDMP4B8g
    nQesFRtXY2ZCjs50Jm73B2LViL9qlREmI6vE5IC8IsRBJSV4ce1WYxyXro5rmVg/
    k6a10rlsbK/eg//GHoJxDdXDOokLUSnxt7gk3QKpX6eCdh67p0PuWm/7WUJQxH2S
    DxsT9vB/iZriTIEe/ILoOQF0Aqp7AgNCcLcLAmbxXQkXYCCSB35Vp06u+eTWjG0/
    pyS5V14stGtw+fA0DJp5ZJV4eqJ5LqxMlYvEZ/qKTEdoCeaXv2QEmN6dVqjDoTAo
    k0t5u4YRXzEVCfXAC3ocplNdtCA72wjFJcSbfif4BSC8bDACTXtnPC7nD0VndZLp
    +RiNLeiENhk0oTC+UVdSc+n2nJOzkCK0vYu0Ads4JGIB7g8IB3z2t9ICmsWrgnhd
    NdcOe15BincrGA8avQ1cWXsfIKEjbrnEuEk9b5jel6NfHtPKoHc9mDpRdNPISeVa
    wDBM1mJChneHt59Nh8Gah74+TM1jBsw4fhJPvoc7Atcg740JErb904mZfkIEmojC
    VPhBHVQ9LHBAdM8qFI2kRK0IynOmAZhexlP/aT/kpEsEPyaZQlnBn3An1CRz8h0S
    PApL8PytggYKeQmRhl499+6jLxcZ2IegLfqq41dzIjwHwTMplg+1pKIOVojpWA==
    -----END CERTIFICATE-----
    </ca>
    key-direction 1
    <tls-auth>
    #
    # 2048 bit OpenVPN static key
    #
    -----BEGIN OpenVPN Static key V1-----
    e685bdaf659a25a200e2b9e39e51ff03
    0fc72cf1ce07232bd8b2be5e6c670143
    f51e937e670eee09d4f2ea5a6e4e6996
    5db852c275351b86fc4ca892d78ae002
    d6f70d029bd79c4d1c26cf14e9588033
    cf639f8a74809f29f72b9d58f9b8f5fe
    fc7938eade40e9fed6cb92184abb2cc1
    0eb1a296df243b251df0643d53724cdb
    5a92a1d6cb817804c4a9319b57d53be5
    80815bcfcb2df55018cc83fc43bc7ff8
    2d51f9b88364776ee9d12fc85cc7ea5b
    9741c4f598c485316db066d52db4540e
    212e1518a9bd4828219e24b20d88f598
    a196c9de96012090e333519ae18d3509
    9427e7b372d348d352dc4c85e18cd4b9
    3f8a56ddb2e64eb67adfc9b337157ff4
    -----END OpenVPN Static key V1-----
    </tls-auth>
  '';

  systemd.services.flix-index = {
    wantedBy = [ "multi-user.target" ];
    path = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.inotify-tools
    ];
    serviceConfig = {
      Restart = "always";
      ExecStart = pkgs.writers.writeDash "flix-index" ''
        set -efu

        DIR=/var/download
        cd "$DIR"
        while inotifywait -rq -e create -e move -e delete "$DIR"; do
          find . -type f > "$DIR"/index.tmp
          mv "$DIR"/index.tmp "$DIR"/index
        done
      '';
    };
  };

  services.jellyfin = {
    enable = true;
    group = "download";
  };

  services.radarr = {
    enable = true;
  };

  services.sonarr = {
    enable = true;
  };

  services.prowlarr = {
    enable = true;
  };
}
