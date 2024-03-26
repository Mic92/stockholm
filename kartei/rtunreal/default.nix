{ config, lib, ... }:
let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };

  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
    owner = config.krebs.users.rtunreal;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum = {
      ip6.addr = (slib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
    };
  } // optionalAttrs (host.nets?wiregrill) {
    nets.wiregrill = {
      ip6.addr = (slib.krebs.genipv6 "wiregrill" "external" { inherit hostName; }).address;
    };
  });
  ssh-for = name: builtins.readFile (./ssh + "/${name}.pub");
in
{
  users = rec {
    rtunreal = rtunreal-spinner;
    rtunreal-spinner = {
      mail = "unreal@rtinf.net";
      pubkey = ssh-for "rtunreal.spinner";
    };
    rtunreal-runner = {
      inherit (rtunreal-spinner) mail;
      pubkey = ssh-for "rtunreal.runner";
    };
  };
  hosts = mapAttrs hostDefaults {
    rtworker = {
      nets.retiolum = {
        aliases = [ "worker.rtunreal.r" ];
        ip4.addr = "10.243.20.24";
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAs6abzbB+Jljux8ivE8gtuYpTxP3FDcXQ+p8DKwadwXyDwehfyKpJ
          nMMOBPnFzUgrN9zTx7Y1unvzjyxDUoyCBrDv6VLG+cS1wDQJTkovVjqWgZAWEg66
          UteNIxZoTqkQan8UD1Hb2rStpI+1C/fgqc4Kc0KX10sBp9qpsUBWwXqsXGsGOWEm
          zLqjM5o0HnK9s4vKHatxvxe1yNoFiivsYDiiStnx0jXHuWcsDcazJeTYLJAdS9mC
          nMfvpp6QTlDiGxxYzQrImo+KZ5aD3ugtvChK0UyIwdD1FDjgHm/NsZ1spheBhCNA
          ddmS5s5xhwh86wMnbOIdrPYJweGyw4jLX6C3nqO/WJegJsfRFPKcBc0oR2lZ3pk+
          JkcvdbxiqRBEuDjttkGia4TbJ8jFHDfGDAmuIMa0Oa0GmuF+DD3Fm6GTXsKTS2jZ
          sAL3b5wH1iOWCKYQhZmHlqw9DiaywsNKeH+/YtAAArH0Mle1yDP0QwSHvI3xlg5H
          6igmHcK70j0ZYQKQHm5Y6FboUTZUsXkIQFdMOdHUW2KeFVQOHDpnH/dXYW97Z0fU
          e4/SOdveJmF1QOygwhdddcumblBJIk0ljMkIA1fo8D4viCo6Vu0wwZbzAXhZlYkr
          dXVy/bhx6xNjFMsNvmsdGx7NEulyukrApLy3JmVYZEYvcKd5lMmvxksCAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "cm3VTJMq+Z9JDaIAZhgNjs5nYOHzM8m4gglX4YCFfnF";
      };
    };
    rtspinner = {
      nets.retiolum = {
        aliases = [ "spinner.rtunreal.r" ];
        ip4.addr = "10.243.20.18";
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEApgnFW2hCP2Lf+CGMtzgiTyA9sphEKGzVtOTJy+LxZ/WchFU6QiU6
          Dl5ybz/Bor25dbwvQCRsQo42gPb+xyjsoHGu2q1NVazMQobePjt/8Qzfqw+Ydz3e
          CC0Lq2J7A5HkzHAevvSHjWh52EfAfu9PGnsszDyWY/oKY+JkBd3wdnE4VsZIhUU6
          Zrmuq+JU53Wy4TAcd3JNStvTW3z7MK4BXxovTV3zSq9sg4a120dyrG/d/m35abvm
          V20Qb9VPmG+861f7gBn45M1w9d4X+3Ev8zum60Lk9JDRETfnufbOsSWNFVk2nsc3
          wpCYd+7FMq5hBf75At/pQ32kbsMkAMpQDJlHwE/xmhxYU2mzlMLY6JW1gspOt00C
          iny5qqmhMoZ3r1VmGuu1aA00V+My+dj5i+pvZiUQ9DG2eYoKM43Var2XsU6lURpL
          UhozcYkb+ax9mqlaPjq2BSYLNzmqTJc3FJY6CcyZxIi4aB8EhDeebYD7wIX115tf
          wwMIJB9FgmvwBhL2K48P5p8lmxU0sNidvv/Gnr3Fgf1p+jEo8BC9hDK3gigD0lqo
          AGmRrjHQN7AjysTMTllDj8RSoO2LhOYTxVtcMsQnPJ9hfFrgnSpSZok64y0h+QJG
          q2WZRBwRYORC7JfKNbE6drRtM6DXccMxOM0eQXoDvg3D5Xg4aqWy3ikCAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "eHWJxlhbUQY0rT2PLqbqb9W4hf7zHh3+gEIRaGrxAdB";
      };
    };
    rtrunner = {
      nets.retiolum = {
        aliases = [ "runner.rtunreal.r" ];
        ip4.addr = "10.243.20.22";
        tinc.pubkey = ''
          -----BEGIN RSA PUBLIC KEY-----
          MIICCgKCAgEAwWSzslk21TbghFsEWk+A0FobqgxrYyyimzSw772OhIpDmCLd63Vr
          x1A/ytEObngMgv/YDTZrp23uFo9uFipAIZPBFBPDPi3fa8OuaGY6MFP6961Ui30l
          4cHBfhuokfdDZxaARwsUtk3RgvFjQvF//Wgj6MIMg3lBMxr00/U3bhegkhP2NyT6
          NCB9xbM6iJQyzOum49u0NHXUEkDzpHWm85CcyV4UTv+MQEnXU4l2irYFu+ArTPEn
          dHqbKBd8lPuLTH1ehiOTh85qC/KV36jHWwmguR96aVEplrFMgV43VnpJj5jLa1NQ
          n00JiCkCVf89LkAz4ZXtQ+5cvDRSWQGYql+J3KJ28YynLPOIlVlEJ+HjhaSQT/3O
          qiREOjp2KPpnSoY5561J2LfmL+shpsVzyFxO+2P0K2bE5K66LfTfmoLUiHKq4/SR
          8EPBZfwvMyWbL3vxngFhZKI01LMsf0YJxu9FWCOPa2X6B7JAxr1jMn0Uzw3ZvNnq
          q6QK/sJhuM1/ddmCMofKYeOtfdunnboniFzI2QValuIdmlOi7nYNqy+gSrxRSWnJ
          PTzGoJB9R4/PufSGJxUr7FCRxSY/TN7fJF74YVG9iVz2ttEuwdUI3ORQVrORbpEI
          wEtM64cb0Dt2WyB3Sit8UGtK59BPYJcU7PB+tMnNLynPzFdkj8gDZtsCAwEAAQ==
          -----END RSA PUBLIC KEY-----
        '';
        tinc.pubkey_ed25519 = "YJE4KD9PhDjxucDAGrbec5Yqqf3A8/VU0J0NV8EPXuN";
      };
    };
    rtgraphene = {
      nets.wiregrill = {
        aliases = [ "graphene.rtunreal.w" ];
        ip4.addr = "10.244.20.20";
        wireguard.pubkey = "IZ7tnD5ZVqO886hFzk6k92R70p1J6jYvyIEAWUccehU=";
      };
    };
  };
}
