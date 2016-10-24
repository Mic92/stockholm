{ config, lib, pkgs, ... }:

let
  inherit (import <stockholm/lib>) genid;

in {

  users.extraUsers = {
    cbasevpn = rec {
      name = "cbasevpn";
      uid = genid "cbasevpn";
      description = "user for running c-base openvpn";
      home = "/home/${name}";
    };
  };

  users.extraGroups.cbasevpn.gid = genid "cbasevpn";

  environment.systemPackages = [
    pkgs.cifs-utils
  ];

  services.openvpn.servers = {
    c-base = {
      config = ''
        client
        dev tap
        proto tcp
        remote vpn.ext.c-base.org 1194
        resolv-retry infinite
        nobind
        user cbasevpn
        group cbasevpn
        persist-key
        persist-tun

        auth-nocache
        #auth-user-pass
        auth-user-pass ${toString <secrets/cbase.txt>}

        comp-lzo
        verb 3

        #script-security 2
        #up /etc/openvpn/update-resolv-conf
        #down /etc/openvpn/update-resolv-conf

        <ca>
        -----BEGIN CERTIFICATE-----
        MIIDUjCCArugAwIBAgIJAOOk8EXgjsf5MA0GCSqGSIb3DQEBBQUAMHoxCzAJBgNV
        BAYTAkRFMQswCQYDVQQIEwJERTEPMA0GA1UEBxMGQmVybGluMQ8wDQYDVQQKEwZj
        LWJhc2UxGzAZBgNVBAMTEnZwbi5leHQuYy1iYXNlLm9yZzEfMB0GCSqGSIb3DQEJ
        ARYQYWRtYXhAYy1iYXNlLm9yZzAeFw0wOTAyMTMwOTE1MzdaFw0xOTAyMTEwOTE1
        MzdaMHoxCzAJBgNVBAYTAkRFMQswCQYDVQQIEwJERTEPMA0GA1UEBxMGQmVybGlu
        MQ8wDQYDVQQKEwZjLWJhc2UxGzAZBgNVBAMTEnZwbi5leHQuYy1iYXNlLm9yZzEf
        MB0GCSqGSIb3DQEJARYQYWRtYXhAYy1iYXNlLm9yZzCBnzANBgkqhkiG9w0BAQEF
        AAOBjQAwgYkCgYEAt3wEgXbqFKxs8z/E4rv13hkRi6J+QdshNzntm7rTOmUsXKE7
        IEwoJSglrmsDPv4UqE86A7bjW7YYSFjhzxFRkTEHJanyOCF48ZPItVl7Eq7T81co
        uR+6lAhxnLDrwnPJCC83NzAa6lw8U1DsQRDkayKlrQrtZq6++pFFEvZvt1cCAwEA
        AaOB3zCB3DAdBgNVHQ4EFgQUqkSbdXS90+HtqXDeAI+PcyTSSHEwgawGA1UdIwSB
        pDCBoYAUqkSbdXS90+HtqXDeAI+PcyTSSHGhfqR8MHoxCzAJBgNVBAYTAkRFMQsw
        CQYDVQQIEwJERTEPMA0GA1UEBxMGQmVybGluMQ8wDQYDVQQKEwZjLWJhc2UxGzAZ
        BgNVBAMTEnZwbi5leHQuYy1iYXNlLm9yZzEfMB0GCSqGSIb3DQEJARYQYWRtYXhA
        Yy1iYXNlLm9yZ4IJAOOk8EXgjsf5MAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcNAQEF
        BQADgYEAOBANG1H4uEEWk3sbeQoSMeA3LFG1+6MgFGk2WAdeHYuV9GKYBq6/PLP5
        ffw+FNkiDjLSeSQO88vHYJr2V1v8n/ZoCIT+1VBcDWXTpGz0YxDI1iBauO3tUPzK
        wGs46RA/S0YwiZw64MaUHd88ZVadjKy9kNoO3w6/vpAS6s/Mh+o=
        -----END CERTIFICATE-----
        </ca>
        key-direction 1
        <tls-auth>
        #
        # 2048 bit OpenVPN static key
        #
        -----BEGIN OpenVPN Static key V1-----
        5d49aa8c9cec18de7ab6e0b5cd09a368
        d3f1b8b77e055e448804fa0e14f487cb
        491681742f96b54a23fb8639aa9ed14e
        c40b86a5546b888c4f3873f23c956e87
        169076ec869127ffc85353fd5928871c
        da19776b79f723abb366fae6cdfe4ad6
        7ef667b7d05a7b78dfd5ea1d2da276dc
        5f6c82313fe9c1178c7256b8d1d081b0
        4c80bc8f21add61fbc52c158579edc1d
        bbde230afb9d0e531624ce289a17098a
        3261f9144a9a2a6f0da4250c9eed4086
        187ec6fa757a454de743a349e32af193
        e9f8b49b010014bdfb3240d992f2f234
        581d0ce05d4e07a2b588ad9b0555b704
        9d5edc28efde59226ec8942feed690a1
        2acd0c8bc9424d6074d0d495391023b6
        -----END OpenVPN Static key V1-----
        </tls-auth>
      '';
    };
  };
}
