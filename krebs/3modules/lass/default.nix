{ config, ... }:

with import <stockholm/lib>;

{
  dns.providers = {
    "lassul.us" = "zones";
  };
  hosts = mapAttrs (_: recursiveUpdate {
    owner = config.krebs.users.lass;
    ci = true;
    monitoring = true;
  }) {
    prism = rec {
      cores = 4;
      extraZones = {
        "krebsco.de" = ''
          prism     IN A ${nets.internet.ip4.addr}
          paste     IN A ${nets.internet.ip4.addr}
        '';
        "lassul.us" = ''
          $TTL 3600
          @ IN SOA dns16.ovh.net. tech.ovh.net. (2017093001 86400 3600 3600000 300)
                              60 IN NS     ns16.ovh.net.
                              60 IN NS     dns16.ovh.net.
                              60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
                              60 IN TXT    v=spf1 mx a:lassul.us -all
                              60 IN TXT    ( "v=DKIM1; k=rsa; t=s; s=*; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDUv3DMndFellqu208feABEzT/PskOfTSdJCOF/HELBR0PHnbBeRoeHEm9XAcOe/Mz2t/ysgZ6JFXeFxCtoM5fG20brUMRzsVRxb9Ur5cEvOYuuRrbChYcKa+fopu8pYrlrqXD3miHISoy6ErukIYCRpXWUJHi1TlNQhLWFYqAaywIDAQAB" )
          default._domainkey  60 IN TXT    "k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDUv3DMndFellqu208feABEzT/PskOfTSdJCOF/HELBR0PHnbBeRoeHEm9XAcOe/Mz2t/ysgZ6JFXeFxCtoM5fG20brUMRzsVRxb9Ur5cEvOYuuRrbChYcKa+fopu8pYrlrqXD3miHISoy6ErukIYCRpXWUJHi1TlNQhLWFYqAaywIDAQAB"
          cgit                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          go                  60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          io                  60 IN NS     ions.lassul.us.
          ions                60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          paste               60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          lol                 60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
          radio               60 IN A      ${config.krebs.hosts.prism.nets.internet.ip4.addr}
        '';
      };
      nets = rec {
        internet = {
          ip4.addr = "46.4.114.247";
          aliases = [
            "prism.i"
            "paste.i"
          ];
          ssh.port = 45621;
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.0.103";
          ip6.addr = "42:0000:0000:0000:0000:0000:0000:15ab";
          aliases = [
            "prism.r"
            "cache.prism.r"
            "cgit.prism.r"
            "paste.r"
            "p.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEAtpI0+jz2deUiH18T/+JcRshQi7lq8zlRvaXpvyuxJlYCz+o5cLje
            fxrKn67JbDb0cTAiDkI88alHBd8xeq2I6+CY90NT6PNVfsQBFx2v5YXafELXJWlo
            rBvPFrR7nt1VzmG/hzkY8RwgC8hC6jRn7cvWWPCkvm2ZnNtYqAjiYMcUcWv6Vn9Z
            ytPgkebDF9KpD8bL4vQu9iPZGNZpwncCw/Ix66oyTM6e24j/fTYgp7xn28wVUzUB
            wWDH0uMQOxyBGFutEvAQ48XZ+QQxZv+2ZGqWJ+MeXreUPNP5wTxFCQOrkR1EXNio
            /jgdHXtU5wVvqPwziukwwnfGJYUUHw7mjdo6ps5rch/aDxs0lahNc2TMbhr3rqgA
            BkXVfwDTt8W/PB6Z0Y/djXOlUmQKO39OgZuhsYzqM4Uj17up7CDY77SiQYrV901C
            9CR5oFsAvV+WIMFUBc7ZZGPotJ9nZ2yyLQh+fT3sXuqFpGlyaI2SAm2edZUXKWQ5
            Q6AIyQRPkTNRCDuvXxIMdmOE++tBnyCI/Psn/Qet5gFcSsUMPhto8Yaka4SgJfyu
            3iIojFUzskowLWt6dBOGm5brI/OaKz0gyw5K3Hb4T7Jz+EwoeJfhbdZYA6NIY+qH
            TGGl+47ffT+8e+1hvcAnO+bN5Br8WPN3+VD4FQD5yTb6pCFdZuL3QEyoKc9eugDb
            g/+rFOsI8bfVeH5zZrl6B6XJBLGeKEECf3zwE2JObO3IuwxATSkahx1jAEy+hFyZ
            kPwooGj03tkgVGc2AxgdHbfmNUbSVkO+m+ouBojikSrnFNKRTS/wZ69RVg3tl4qg
            7F4Vs/aMQ9bSWycvRBZQXITPQ1Y6mCEUj2mSKVHmgy/5rqwz2va/Yc1zhUptcINo
            7ztGiEzFMPGagkTs/Ntuqh2VbC/MwTao0BKl+gyCNwrACnNW87X4og2gtG3ukduz
            cnSupO84hdTrclthsSEH/rLUauBsuIch58S/F7KCz9hwK45+Btky7Kz4mf/pE451
            k88QfDHw/cTSzlESPnEnthrRnhxn0fW7FRwJpieKm2AmyEEjSiiYt8mUdD3teKj0
            dgYrcGQkCnhmKDawgcw46wstBG/sAKT8qnZPRmlzKpcCS186ffuobQvj42LSmuMu
            ToANi5pw2yEfzwLxNG/3whozB9rqwbqV/YAR/mthMxD0IXpLDKXlV1IeD7MfpV8i
            jx6SghnkX/s2F7UTOlwJYe/Gl1biLRB8EPnOZKadHR0BRWFd+Qz6pJDp0B13jT3/
            AEPNGXLwVjmdhy2TVec3OGL/CukPEdiW1Urw5lfOc9dacTXjTNTXzod7Ub6s7ZOE
            T7Y4dsVeW4OM7NmE/riqS3cG9obGWO7gIQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAsANFdMi825qWQXQbWLYuNZ6/fARt3lnh1KStQHQQMD";
    };
    domsen-nas = {
      ci = false;
      monitoring = false;
      external = true;
      nets = rec {
        internet = {
          aliases = [
            "domsen-nas.internet"
          ];
          ip4.addr = "87.138.180.167";
          ssh.port = 2223;
        };
      };
    };
    uriel = {
      monitoring = false;
      cores = 1;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.12";
          aliases = ["uriel.gg23"];
          ssh.port = 45621;
        };
        retiolum = {
          ip4.addr = "10.243.81.176";
          ip6.addr = "42:dc25:60cf:94ef:759b:d2b6:98a9:2e56";
          aliases = [
            "uriel.r"
            "cgit.uriel.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzw0pvoEmqeqiZrzSOPH0IT99gr1rrvMZbvabXoU4MAiVgGoGrkmR
            duJkk8Fj12ftMc+Of1gnwDkFhRcfAKOeH1RSc4CTircWVq99WyecTwEZoaR/goQb
            MND022kIBoG6NQNxv1Y5I1B/h7hfloMFEPym9oFtOAXoGhBY2vVl4g64NNz+RLME
            m1RipLXKANAh6LRNPGPQCUYX4TVY2ZJVxM3CM1XdomUAdOYXJmWFyUg9NcIKaacx
            uRrmuy7J9yFBcihZX5Y7NV361kINrpRmZYxJRf9cr0hb5EkJJ7bMIKQMEFQ5RnYo
            u7MPGKD7aNHa6hLLCeIfJ5u0igVmSLh3pwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBryIo/Waw8SWvlQ0+5I+Bd/dJgcMd6iPXtELS6gQXoc";
      secure = true;
    };
    mors = {
      cores = 2;
      nets = {
        gg23 = {
          ip4.addr = "10.23.1.11";
          aliases = ["mors.gg23"];
          ssh.port = 45621;
        };
        retiolum = {
          ip4.addr = "10.243.0.2";
          ip6.addr = "42:0:0:0:0:0:0:dea7";
          aliases = [
            "mors.r"
            "cgit.mors.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAsj1PCibKOfF68gmFQ+wwyfhUWpqKqpznrJX1dZ+daae7l7nBHvsE
            H0QwkiMmk3aZy1beq3quM6gX13aT+/wMfWnLyuvT11T5C9JEf/IS91STpM2BRN+R
            +P/DhbuDcW4UsdEe6uwQDGEJbXRN5ZA7GI0bmcYcwHJ9SQmW5v7P9Z3oZ+09hMD+
            1cZ3HkPN7weSdMLMPpUpmzCsI92cXGW0xRC4iBEt1ZeBwjkLCRsBFBGcUMuKWwVa
            9sovca0q3DUar+kikEKVrVy26rZUlGuBLobMetDGioSawWkRSxVlfZvTHjAK5JzU
            O6y6hj0yQ1sp6W2JjU8ntDHf63aM71dB9QIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMPlIG+6u75GJ3kvsPF6OoIZsU+u8ZQ+rdviv5fNMD";
    };
    shodan = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.4";
          ip6.addr = "42:0:0:0:0:0:0:50d4";
          aliases = [
            "shodan.r"
            "cgit.shodan.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA9bUSItw8rEu2Cm2+3IGHyRxopre9lqpFjZNG2QTnjXkZ97QlDesT
            YYZgM2lBkYcDN3/LdGaFFKrQQSGiF90oXA2wFqPuIfycx+1+TENGCzF8pExwbTd7
            ROSVnISbghXYDgr3TqkjpPmnM+piFKymMDBGhxWuy1bw1AUfvRzhQwPAvtjB4VvF
            7AVN/Z9dAZ/LLmYfYq7fL8V7PzQNvR+f5DP6+Eubx0xCuyuo63bWuGgp3pqKupx4
            xsixtMQPuqMBvOUo0SBCCPa9a+6I8dSwqAmKWM5BhmNlNCRDi37mH/m96av7SIiZ
            V29hwypVnmLoJEFiDzPMCdiH9wJNpHuHuQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC9vup68R0I+62FK+8LNtwM90V9P4ukBmU7G7d54wf4C";
    };
    icarus = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.114";
          ip6.addr = "42:0:0:0:0:0:01ca:1205";
          aliases = [
            "icarus.r"
            "cgit.icarus.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAydCY+IWzF8DocCNzPiUM+xccbiDTWS/+r2le812+O4r+sUojXuzr
            Q4CeN+pi2SZHEOiRm3jO8sOkGlv4I1WGs/nOu5Beb4/8wFH6wbm4cqXTqH/qFwCK
            7+9Bke8TUaoDj9E4ol9eyOx6u8Cto3ZRAUi6m1ilrfs1szFGS5ZX7mxI73uhki6t
            k6Zb5sa9G8WLcLPIN7tk3Nd0kofd/smwxSN0mXoTgbAf1DZ3Fnkgox/M5VnwpPW7
            zLzbWNFyLIgDGbQ5vZBlJW7c4O0KrMlftvEQ80GeZXaKNt6UK7LSAQ4Njn+8sXTt
            gl0Dx29bSPU3L8udj0Vu6ul7CiQ5bZzUCQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOPgQIMYiyD4/Co+nlOQWEzCKssemOEXAY/lbIZZaMhj";
    };
    daedalus = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.115";
          ip6.addr = "42:0:0:0:0:0:daed:a105";
          aliases = [
            "daedalus.r"
            "cgit.daedalus.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEAzlIJfYIoQGXishIQGFNOcaVoeelqy7a731FJ+VfrqeR8WURQ6D+8
            5hz7go+l3Z7IhTc/HbpGFJ5QJJNFSuSpLfZVyi+cKAUVheTivIniHFIRw37JbJ4+
            qWTlVe3uvOiZ0cA9S6LrbzqAUTLbH0JlWj36mvGIPICDr9YSEkIUKbenxjJlIpX8
            ECEBm8RU1aq3PUo/cVjmpqircynVJBbRCXZiHoxyLXNmh23d0fCPCabEYWhJhgaR
            arkYRls5A14HGMI52F3ehnhED3k0mU8/lb4OzYgk34FjuZGmyRWIfrEKnqL4Uu2w
            3pmEvswG1WYG/3+YE80C5OpCE4BUKAzYSwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAq5Ovdcsljr5dOl7+2sQNKpGpdX0SlOIuCZKEiWEp8g";
    };
    skynet = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.133.116";
          ip6.addr = "42:0:0:0:0:0:0:1101";
          aliases = [
            "skynet.r"
            "cgit.skynet.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEArNpBoTs7MoaZq2edGJLYUjmoLa5ZtXhOFBHjS1KtQ3hMtWkcqpYX
            Ic457utOSGxTE+90yXXez2DD9llJMMyd+O06lHJ7CxtbJGBNr3jwoUZVCdBuuo5B
            p9XfhXU9l9fUsbc1+a/cDjPBhQv8Uqmc6tOX+52H1aqZsa4W50c9Dv5vjsHgxCB0
            yiUd2MrKptCQTdmMM9Mf0XWKPPOuwpHpxaomlrpUz07LisFVGGHCflOvj5PAy8Da
            NC+AfNgR/76yfuYWcv4NPo9acjD9AIftS2c0tD3szyHBCGaYK/atKzIoBbFbOtMb
            mwG3B0X3UdphkqGDGsvT+66Kcv2jnKwL0wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEB/MmASvx3i09DY1xFVM5jOhZRZA8rMRqtf8bCIkC+t";
    };
    littleT = {
      cores = 2;
      nets = {
        retiolum = {
          ip4.addr = "10.243.133.77";
          ip6.addr = "42:0:0:0:0:0:717:7137";
          aliases = [
            "littleT.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEA2nPi6ui8nJhEL3lFzDoPelFbEwFWqPnQa0uVxLAhf2WnmT/vximF
            /m2ZWpKDZyKx17GXQwm8n0NgyvcemvoCVGqSHIsbxvLB6aBF6ZLkeKyx1mZioEDY
            1MWR+yr42dFn+6uVTxJhLPmOxgX0D3pWe31UycoAMSWf4eAhmFIEFUvQCAW43arO
            ni1TFSsaHOCxOaLVd/r7tSO0aT72WbOat84zWccwBZXvpqt/V6/o1MGB28JwZ92G
            sBMjsCsoiciSg9aAzMCdjOYdM+RSwHEHI9xMineJgZFAbQqwTvK9axyvleJvgaWR
            M9906r/17tlqJ/hZ0IwA6X+OT4w/JNGruy/5phxHvZmDgvXmYD9hf2a6JmjOMPp/
            Zn6zYCDYgSYugwJ7GI39GG7f+3Xpmre87O6g6WSaMWCfdOaAeYnj+glP5+YvTLpT
            +cdN9HweV27wShRozJAqTGZbD0Nfs+EXd0J/q6kP43lwv6wyZdmXCShPF2NzBlEY
            xdtWKhRYKC1cs0Z2nK+XGEyznNzp1f8NC5qvTguj4kDMhoOd6WXwk460HF49Tf/c
            aGQTGzgEVMAI7phTJubEmxdBooedvPFamS5wpHTmOt9dZ3qbpCgThaMblVvUu/lm
            7pkPgc60Y2RAk/Rvyy5A8AaxBXPRBNwVkM5TY/5TW+S1zY09600ZCC2GE27qGT9v
            k4GHabO42n3wTHk+APodzKDBbEazhOp5Oclg4nNKqgg+IrmheB91oEqBXlfyDj8B
            idVoUvbH9WPwBqdh7hoqzrHDur5wCFBphrkjEe98o5iFFFi2C8W04H7iqe+nFqvJ
            y/vzKk5kbfpjov71EEje+hNUCLTWF7sjgT4Z2z8LuqjpIq+d2i5dASfTqj4VBs6D
            SeiHyyAfCHG/03I9E5eizCCd98Tr30yhu3IKsdFFXsVwxHVFenq2Y1ca7uypCk+i
            mDC5q5WQFEK/8SSO25i1teWBawfNVVVI/A1b676VJyafS9ebJs8TmXYRbE6rcBzH
            PssdHNwbtEwhbGdQhgQ2pqQg1SIZM3zvjcpgzL9QP29tulubJ05keaw/4p/Yg/mB
            ivF8EAIefXYYVxYkRQsHox7UQpSCzjOtj7gvc0KdJxshSLuryM0LxP+gk+x6JPX5
            Ht8x+oE7iL0cqBsIenc/e0XdTZ+4zrBY5hWbGH8a8VJqEYs54WRJhzQf1jzNaCbS
            8328MpRF5lXujv61aveg0i4pvczznlSV7wXmmwNAdhvSUTh34tCpRqabpCJdlRBt
            NvVuij6guPKt4XV1TxXNsPCfib1vYjvwX8gUE4UhL69VmM8OBaC3XdroMfNvz9YW
            5ObxDGIEiP53Jp8hiWId0AI/XF5Ct3Gh2wIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJzb9BPFClubs6wSOi/ivqPFVPlowXwAxBS0jHaB29hX";
    };
    iso = {
      monitoring = false;
      ci = false;
      cores = 1;
    };
    sokrateslaptop = {
      monitoring = false;
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.142.104";
          ip6.addr = "42:f8a1:044d:0f75:9d73:56d8:f432:c6cc";
          aliases = [
            "sokrateslaptop.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0EMbBv5NCSns4V/VR/NJHhwe2qNLUYjWWtCDY4zDuoiJdm3JNZJ2
            t0iKNxFwd6Mmg3ahAlndsH4FOjOBGBQCgBG25VRnQgli1sypI/gYTsSgIWHVIRoZ
            rgrng0K3oyJ6FuTP+nH1rd7UAYkrOQolXQBY+LqAbxOVjiJl+DpbAXIxCIs5TBeW
            egtBiXZ1S53Lv5EGFXug716XlgZLHjw7PzRLJXSlvUAIRZj0Sjq4UD9VrhazM9s5
            aDuxJIdknccEEXm6NK7a51hU/o8L+T0IUpZxhaXOdi6fvO/y3TbffKb1yRTbN0/V
            VBjBh18Le7h0SmAEED5tz7NOCrAjMZQtJQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    turingmachine = {
      monitoring = false;
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.168";
          ip6.addr = "42:4992:6a6d:600::1";
          aliases = [
            "turingmachine.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAxh+5HD1oAFTvMWEra2pYrA3HF8T4EnkP917lIUiuN7xUj7sawu0C
            t1/1IfIlH9dbxgFe5CD/gXvokxHdovPTGVH11L+thZgq6hg/xbYvZAl76yLxj7t9
            6+Ocac08TQZYMqWKShz5jqTVE/DLz4Cdy0Qk9sMJ1++OmH8jsWgK5BkogF99Gwf8
            ZiI0t3n3lCZsm3v592lveDcVIh6hjuCIvFVxc+7cOj0MKm1LxLWbCHZlUIE3he4g
            nZu4XiYaE4Y2LicMs8zKehnQkkXrP1amT56SqUfbSnWR+HZc2+KjwRDI5BPeTS06
            5WHwkQs0ScOn7vFZci3rElIc7vilu2eKGF1VLce9kXw9SU2RFciqavaEUXbwPnwT
            1WF35Ct+qIOP0rXoObm6mrsj7hJnlBPlVpb58/kTxLHMSHPzqQRbFZ35f6tZodJ1
            gRMKKEnMX8/VWm6TqLUIpFCCTZ5PH1fxaAnulHCxksK03UyfUOvExCTU4x8KS9fl
            DIoLlV9PFBlAW8mTuIgRKYtHacsc31/5Tehcx0If09NuMFT9Qfl2/Q3p6QJomRFL
            W5SCP9wx2ONhvZUkRbeihBiTN5/h3DepjOeNWd1DvE6K0Ag8SXMyBGtyKfer4ykW
            OR0iCiRQQ5QBmNuJrBLRUyfoPqFUXBATT1SrRj8vzXO1TjTmANEMFD0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    eddie = {
      monitoring = false;
      ci = false;
      external = true;
      nets = rec {
        internet = {
          # eddie.thalheim.io
          ip4.addr = "129.215.197.11";
          aliases = [ "eddie.i" ];
        };
        retiolum = rec {
          via = internet;
          addrs = [
            ip4.addr
            ip6.addr
          ];
          ip4.addr = "10.243.29.170";
          ip6.addr = "42:4992:6a6d:700::1";
          aliases = [ "eddie.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuRQphRlSIC/aqRTfvStPdJOJCx1ACeFIDEjRdgoxuu32qoBl7i6d
            j7Voh+Msditf2a5+f0fVsNDaPnjPGfk0NkZBjmn+RZQDRXk0krpTNj2Vb6W5quTm
            3yrjJMFJR9CU5khfppc47X+ir8bjn7RusWTFNEuDvUswHmRmnJHLS3Y+utOaRbCF
            2hxpyxCn423gpsaBfORPEK8X90nPbuNpFDugWPnC+R45TpNmIf4qyKvfhd9OKrua
            KNanGHG30xhBW/DclUwwWi8D44d94xFnIRVcG1O+Uto93WoUWZn90lI1qywSj5Aq
            iWstBK4tc7VwvAj0UzPlaRYYPfFjOEkPQzj8xC6l/leJcgxkup252uo6m1njMx3t
            6QWMgevjqosY22OZReZfIwb14aDWFKLTWs30J+zmWK4TjlRITdsOEKxlpODMbJAD
            kfSoPwuwkWIzFhNOrFiD/NtKaRYmV8bTBCT3a9cvvObshJx13BP+IUFzBS1N1n/u
            hJWYH5WFsQZn/8rHDwZGkS1zKPEaNoydjqCZNyJpJ5nhggyl6gpuD7wpXM/8tFay
            pAjRP40+qRQLUWXmswV0hsZTOX1tvZs4f68y3WJ+GwCWw9HvvwmzYes5ayJrPsbJ
            lyK301Jb42wGEsVWxu3Eo/PLtp8OdD+Wdh6o/ELcc0k/YCUGFMujUM8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.subnets = [
            # edinburgh university
            "129.215.0.0/16"
          ];
        };
      };
    };
    borg = {
      monitoring = false;
      ci = false;
      external = true;
      nets = {
        retiolum = {
          ip4.addr = "10.243.29.171";
          ip6.addr = "42:4992:6a6d:700::2";
          aliases = [ "borg.r" ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA0bHZApTM7Hl4qqNakSwq
            bt7zJoTVK9ePoC3Mue1VmJ1mCKMaxKdzlO31kPeHtkilAzgyIJdgikyKFlApGsQL
            aIuU9h55X7TbikoDD6ghbSrAe3Pgc+sJ3OZ7wO7Qb8CKgJvEbkk/u68YiJgyTjYD
            HNjIQzlsGdpoSke9vwC8qWanfgN7c2MMGtakqfXDjYjCgp7O43i+SMupkMSXIXMA
            5XUFh/vVp6xgPxBofcw0uQIyZ5v4PPFjnGPm4rnMbFzbhubntHjDadwGd5Niyw4O
            zNNKNchTLfNiuNGqTZeYd0kJ5fNMKykhpSs+ou34MvexvpuyPlFuotnPXN/nOMml
            3nwiqzthzPuBZRLswxT0WvlA8wlbeTOKJ0wTIR4dDuAF+euDtoNocVEN5PJNc7yN
            fmwAV6geESoJbZQMSCtAp1NioaBlRPp1pFfoM/GotHywuFrTIxyoIBiYhkpWyQvq
            WYw5j13IKqkL7jDchhoBmcardmh+AP5bL3uQ84BgaYNwFzHp04qIRrrdpF0eMaHB
            /8zaqsNLn4/zQJB5ffkelwoIqfvLPQeCMLzHGHgP5xUnWgmZZGiiDLvhuaMeNq4U
            EpCKoTL178sPOgNfHfd8mEqx0qKYuPrNQEdlpa5xOZqwx56pfYpGWY+KtF2FHLhS
            iO64GCJqCi1MKBYx/NhaxKMCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
    };
    inspector = {
      monitoring = false;
      ci = false;
      external = true;
      nets = rec {
        internet = {
          ip4.addr = "141.76.44.154";
          aliases = [ "inspector.i" ];
        };
        retiolum = {
          via = internet;
          ip4.addr = "10.243.29.172";
          ip6.addr = "42:4992:6a6d:800::1";
          aliases = [ "inspector.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAr3l/u7qcxmFa2hUICU3oPDhB2ij2R3lKHyjSsVFVLNfl6TpOdppG
            EDXOapeXL0s+PfBRHdRI3v/dibj4PG9eyKmFxsUJ2gRz4ghb1UE23aQ3pkr3x8sZ
            7GR+nJYATYf+jolFF9O1x+f0Uo5xaYWkGOMH8wVVzm6+kcsZOYuTEbJAsbTRZywF
            m1MdRfk54hLiDsj2rjGRZIR+ZfUKVs2MTWOLCpBAHLJK+r3HfUiR2nAgeNkJCFLw
            WIir1ftDIViT3Ly6b7enaOkVZ695FNYdPWFZCE4AJI0s9wsbMClzUqCl+0mUkumd
            eRXgWXkmvBsxR4GECnxUhxs6U8Wh3kbQavvemt4vcIKNhkw32+toYc1AFK/n4G03
            OUJBbRqgJYx9wIvo8PEu4DTTdsPlQZnMwiaKsn+Gi4Ap6JAnG/iLN8sChoQf7Dau
            ARZA3sf9CkKx5sZ+9dVrLbzGynKE18Z/ysvf1BLd/rVVOps1B/YRBxDwPj8MZJ0x
            B7b0j+hRVV5palp3RRdcExuWaBrMQQGsXwLUZOFHJJaZUHF9XRdy+5XVJdNOArkG
            q1+yGhosL1DLTQE/VwCxmBHyYTr3L7yZ2lSaeWdIeYvcRvouDROUjREVFrQjdqwj
            7vIP1cvDxSSqA07h/xEC4YZKACBYc/PI2mqYK5dvAUG3mGrEsjHktPUCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    dpdkm = {
      monitoring = false;
      ci = false;
      external = true;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.29.173";
          ip6.addr = "42:4992:6a6d:900::1";
          aliases = [ "dpdkm.r" ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAuW31xGBdPMSS45KmsCX81yuTcDZv1z7wSpsGQiAw7RsApG0fbBDj
            NvzWZaZpTTUueG7gtt7U9Gk8DhWYR1hNt8bLXxE5QlY+gxVjU8+caRvlv10Y9XYp
            qZEr1n1O5R7jS1srvutPt74uiA8I3hBoeP5TXndu8tVcehjRWXPqJj4VCy9pT2gP
            X880Z30cXm0jUIu9XKhzQU2UNaxbqRzhJTvFUG04M+0a9olsUoN7PnDV6MC5Dxzn
            f0ZZZDgHkcx6vsSkN/C8Tik/UCXr3tS/VX6/3+PREz6Z3bPd2QfaWdowrlFQPeYa
            bELPvuqYiq7zR/jw3vVsWX2e91goAfKH5LYKNmzJCj5yYq+knB7Wil3HgBn86zvL
            Joj56VsuB8fQrrUxjrDetNgtdwci+yFeXkJouQRLM0r0W24liyCuBX4B6nqbj71T
            B6rAMzhBbl1yixgf31EgiCYFSusk+jiT+hye5lAhes4gBW9GAWxGNU9zE4QeAc1w
            tkPH/CxRIAeuPYNwmjvYI2eQH9UQkgSBa3/Kz7/KT9scbykbs8nhDHCXwT6oAp+n
            dR5aHkuBrTQOCU3Xx5ZwU5A0T83oLExIeH8jR1h2mW1JoJDdO85dAOrIBHWnjLls
            mqrJusBh2gbgvNqIrDaQ9J+o1vefw1QeSvcF71JjF1CEBUmTbUAp8KMCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
    };
    xerxes = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.1.3";
          ip6.addr = "42::1:3";
          aliases = [
            "xerxes.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEArqEaK+m7WZe/9/Vbc+qx2TjkkRJ9lDgDMr1dvj98xb8/EveUME6U
            MZyAqNjLuKq3CKzJLo02ZmdFs4CT1Hj28p5IC0wLUWn53hrqdy8cCJDvIiKIv+Jk
            gItsxJyMnRtsdDbB6IFJ08D5ReGdAFJT5lqpN0DZuNC6UQRxzUK5fwKYVVzVX2+W
            /EZzEPe5XbE69V/Op2XJ2G6byg9KjOzNJyJxyjwVco7OXn1OBNp94NXoFrUO7kxb
            mTNnh3D+iB4c3qv8woLhmb+Uh/9MbXS14QrSf85ou4kfUjb5gdhjIlzz+jfA/6XO
            X4t86uv8L5IzrhSGb0TmhrIh5HhUmSKT4RdHJom0LB7EASMR2ZY9AqIG11XmXuhj
            +2b5INBZSj8Cotv5aoRXiPSaOd7bw7lklYe4ZxAU+avXot9K3/4XVLmi6Wa6Okim
            hz+MEYjW5gXY+YSUWXOR4o24jTmDjQJpdL83eKwLVAtbrE7TcVszHX6zfMoQZ5M9
            3EtOkDMxhC+WfkL+DLQAURhgcPTZoaj0cAlvpb0TELZESwTBI09jh/IBMXHBZwI4
            H1gOD5YENpf0yUbLjVu4p82Qly10y58XFnUmYay0EnEgdPOOVViovGEqTiAHMmm5
            JixtwJDz7a6Prb+owIg27/eE1/E6hpfXpU8U83qDYGkIJazLnufy32MTFE4T9fI4
            hS8icFcNlsobZp+1pB3YK4GV5BnvMwOIVXVlP8yMCRTDRWZ4oYmAZ5apD7OXyNwe
            SUP2mCNNlQCqyjRsxj5S1lZQRy1sLQztU5Sff4xYNK+5aPgJACmvSi3uaJAxBloo
            4xCCYzxhaBlvwVISJXZTq76VSPybeQ+pmSZFMleNnWOstvevLFeOoH2Is0Ioi1Fe
            vnu5r0D0VYsb746wyRooiEuOAjBmni8X/je6Vwr1gb/WZfZ23EwYpGyakJdxLNv3
            Li+LD9vUfOR80WL608sUU45tAx1RAy6QcH/YDtdClbOdK53+cQVTsYnCvDW8uGlO
            scQWgk+od3qvo6yCPO7pRlEd3nedcPSGh/KjBHao6eP+bsVERp733Vb9qrEVwmxv
            jlZ1m12V63wHVu9uMAGi9MhK+2Q/l7uLTj03OYpi4NYKL2Bu01VXfoxuauuZLdIJ
            Z3ZV+qUcjzZI0PBlGxubq6CqVFoSB7nhHUbcdPQ66WUnwoKq0cKmE7VOlJQvJ07u
            /Wsl8BIsxODVt0rTzEAx0hTd5mJCX7sCawRt+NF+1DZizl9ouebNMkNlsEAg4Ps0
            bQerZLcOmpYjGa5+lWDwJIMXVIcxwTmQR86stlP/KQm0vdOvH2ZUWTXcYvCYlHkQ
            sgVnnA2wt+7UpZnEBHy04ry+jYaSsPdYgwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE5HyLyaIvVH0qHIQ4ciKhDiElhSqsK+uXcA6lTvL+5n";
    };
    cabal = {
      cores = 2;
      nets = rec {
        retiolum = {
          ip4.addr = "10.243.1.4";
          ip6.addr = "42::1:4";
          aliases = [
            "cabal.r"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIECgKCBAEAukXm8xPpC6/F+wssYqQbqt1QDwsPrF3TJ9ToLFcN1WgDlhDhjM3A
            SuRDMNjRT1fvVTuXyplH5g16eokW/yLOpNnznMS3/VR372pLPEOqfuRf7wAy18jj
            rZkW3EO7nyZ8KMb+SXA8Q0KIpHY50Ezh+tqGoTZDICwoK6N5dKLgAZShS55JXwwK
            qRG3vyzV3mDjgVyT0FNfyL1/BN1qvJ+tQQ40lEbkcQauMunMzNbH058kAd6H2/0e
            LK4JkxI9XpZHE6Pf1epXyClHW7vT7APFRp9gL9tZS/XMC18+aEMFfQrNW9jb3FIq
            rU5MfJ7aubboe7dT6CRaRSWpduiKLVzY/JCoGvUziyvmR7qHsQWTEjtNuQX9joc3
            6iq1o+gmLV0G8Xwq8cEcg5USlLxNsGBQPwYnTG6iTPPHqOv7BKucekE/opnVZseE
            fSNCGl1+tGwa3soSMI97LkpQTZxdeqf+jWZve0RbSa2Ihyod91ldFCqi1+PZx68v
            yBI0PJamlt+dBx6WQKbPngWYeD8hXo7tg0XVRVa3ZQyX+Mq6uCCb2GM8ewMUPl+A
            kcY1osFt6+sdkFGdiv3FMyijAiZumPoPprXC/4SGIsMnkoI4JfSAbTpHi2QuesqR
            KMeairdB7XGUYlMvWpDLKN2dbMdRc+l3kDUKT7hALjKeyWS/27WYeK/STxvZXEXi
            TZGHopvOFv6wcrb6nI49vIJo5mDLFamAPN3ZjeR20wP95UP7cUUSaTYX49M4lX6U
            oL5BaFrcLn2PTvS84pUxcXKAp70FgTpvGJbaWwETgDjW+H+qlGmI/BTejpL7flVs
            TOtaP/uCMxhVZSFv9bzo0ih10o+4gtU8lqxfJsVxlf2K7LVZ++LQba/u+XxRY+xw
            3IFBfg34tnO6zYlV8XgAiJ6IUOHUZANsuBD4iMoFSVOig6t5eIOkgXR6GEkP8FBD
            rkroRMmxcu4lTCOzWIuAVOxCd4XXguoGQ4HAzpGd5ccdcb8Ev4RYEvNJY7B5tIQZ
            4J0F9ECzJuSu1HvWTL+T6a36d2MDTkXU2IJ2tSHciXqiP+QMMF7p9Ux0tiAq4mtf
            luA94uKWg3cSyTyEM/jF66CgO6Ts3AivNE0MRNupV6AbUdr+TjzotGn9rxi168py
            w/49OVbpR9EIGC2wxx7qcSEk5chFOcgvNQMRqgIx51bbOL7JYb0f4XuA38GUqLkG
            09PXmPeyqGzR9HsV2XZDprZdD3Dy4ojdexw0+YILg9bHaAxLHYs6WFZvzfaLLsf1
            K2I39vvrEEOy8tHi4jvMk7oVX6RWG+DOZMeXTvyUCaBHyYkA0eDlC6NeKOHxnW/g
            ZtN1W93UdklEqc5okM0/ZIke1HDRt3ZLdQIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      secure = true;
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPsTeSAedrbp7/KmZX8Mvka702fIUy77Mvqo9HwzCbym";
    };
    red = {
      monitoring = false;
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.13";
          ip6.addr = "42:0:0:0:0:0:0:12ed";
          aliases = [
            "red.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEArAN/62V2MV18wsZ9VMTG
            4/cqsjvHlffAN8jYDq+GImgREvbiLlFhhHgxwKh0gcDTR8P1xX/00P3/fx/g5bRF
            Te7LZT2AFmVFFFfx1n9NBweN/gG2/hzB9J8epbWLNT+RzpzHuAoREvDZ+jweSXaI
            phdmQY2s36yrR3TAShqq0q4cwlXuHT00J+InDutM0mTftBQG/fvYkBhHOfq4WSY0
            FeMK7DTKNbsqQiKKQ/kvWi7KfTW0F0c7SDpi7BLwbQzP2WbogtGy9MIrw9ZhE6Ox
            TVdAksPKw0TlYdb16X/MkbzBqTYbxFlmWzpMJABMxIVwAfQx3ZGYvJDdDXmQS2qa
            mDN2xBb/5pj3fbfp4wbwWlRVSd/AJQtRvaNY24F+UsRJb0WinIguDI6oRZx7Xt8w
            oYirKqqq1leb3EYUt8TMIXQsOw0/Iq+JJCwB+ZyLLGVNB19XOxdR3RN1JYeZANpE
            cMSS3SdFGgZ//ZAdhIN5kw9yMeKo6Rnt+Vdz3vZWTuSVp/xYO3IMGXNGAdIWIwrJ
            7fwSl/rfXGG816h0sD46U0mxd+i68YOtHlzOKe+vMZ4/FJZYd/E5/IDQluV8HLwa
            5lODfZXUmfStdV+GDA9KVEGUP5xSkC3rMnir66NgHzKpIL002/g/HfGu7O3MrvpW
            ng7AMvRv5vbsYcJBj2HUhKUCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKd/6eCR8yxC14zBJLIQgVa4Zbutv5yr2S8k08ztmBpp";
    };
    blue = {
      cores = 1;
      nets = {
        retiolum = {
          ip4.addr = "10.243.0.77";
          ip6.addr = "42:0:0:0:0:0:0:77";
          aliases = [
            "blue.r"
          ];
          tinc.pubkey = ''
            -----BEGIN PUBLIC KEY-----
            MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA28b+WMiQaWbwUPcJlacd
            QwyX4PvVm9WItPmmNy+RE2y0Mf04LxZ7RLm5+e0wPuhXXQyhZ06CNd6tjeaKfXUc
            sNeC1Vjuh1hsyYJLR5Xf/YRNJQKoaHjbkXGt+rSK7PPuCcsUPOSZSEAgHYVvcFzM
            wWE4kTDcBZeISB4+yLmPIZXhnDImRRMEurFNRiocoMmEIu/zyYVq8rnlTl972Agu
            PMGo1HqVxCouEWstRvtX5tJmV8yruRbH4tADAruLXErLLwUAx/AYDNRjY1TYYetJ
            RoaxejmZVVIvR+hWaDLkHZO89+to6wS5IVChs1anFxMNN6Chq2v8Bb2Nyy1oG/H/
            HzXxj1Rn7CN9es5Wl0UX4h9Zg+hfspoI75lQ509GLusYOyFwgmFF02eMpxgHBiWm
            khSJzPkFdYJKUKaZI0nQEGGsFJOe/Se5jj70x3Q5XEuUoQqyahAqwQIYh6uwhbuP
            49RBPHpE+ry6smhUPLTitrRsqeBU4RZRNsUAYyCbwyAH1i+K3Q5PSovgPtlHVr2N
            w+VZCzsrtOY2fxXw0e+mncrx/Qga62s4m6a/dyukA5RytA9f6bBsvSTqr7/EQTs6
            ZEBoPudk7ULNEbfjmJtBkeG7wKIlpgzVg/JaCAwMuSgVjrpIHrZmjOVvmOwB8W6J
            Ch/o7chVljAwW4JmyRnhZbMCAwEAAQ==
            -----END PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.id_ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILSBxtPf8yJfzzI7/iYpoRSc/TT+zYmE/HM9XWS3MZlv";
    };
  };
  users = rec {
    lass = lass-blue;
    lass-blue = {
      mail = "lass@blue.r";
      pubkey = builtins.readFile ./ssh/blue.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/blue.pgp;
    };
    lass-mors = {
      mail = "lass@mors.r";
      pubkey = builtins.readFile ./ssh/mors.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/mors.pgp;
    };
    lass-android = {
      mail = "lassulus@gmail.com";
      pubkey = builtins.readFile ./ssh/android.rsa;
    };
    lass-helios = {
      mail = "lass@helios.r";
      pubkey = builtins.readFile ./ssh/helios.rsa;
    };
    lass-uriel = {
      mail = "lass@uriel.r";
      pubkey = builtins.readFile ./ssh/uriel.rsa;
    };
    lass-shodan = {
      mail = "lass@shodan.r";
      pubkey = builtins.readFile ./ssh/shodan.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/shodan.pgp;
    };
    lass-icarus = {
      mail = "lass@icarus.r";
      pubkey = builtins.readFile ./ssh/icarus.rsa;
      pgp.pubkeys.default = builtins.readFile ./pgp/icarus.pgp;
    };
    lass-xerxes = {
      mail = "lass@xerxes.r";
      pubkey = builtins.readFile ./ssh/xerxes.rsa;
    };
    lass-daedalus = {
      mail = "lass@daedalus.r";
      pubkey = builtins.readFile ./ssh/daedalus.rsa;
    };
    fritz = {
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCz34435NSXgj72YAOL4cIlRq/4yInKEyL9no+gymURoW5x1nkYpP0EK331e7UyQQSOdWOogRo6d7YHcFqNlYWv5xlYcHucIhgJwC4Zda1liVA+v7tSOJz2BjmFvOT3/qlcPS69f3zdLHZooz2C33uHX1FgGRXlxiA8dpqGnSr8o76QLZjuQkuDqr8reOspjO/RHCo2Moq0Xm5q9OgN1WLAZzupqt9A5lx567mRzYsRAr23pUxVN8T/tSCgDlPe4ktEjYX9CXLKfMyh9WuBVi+AuH4GFEWBT+AMpsHeF45w+w956x56mz0F5nYOQNK87gFr+Jr+mh2AF1ot2CxzrfTb fritz@scriptkiddiT540";
    };
    prism-repo-sync = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKhpCKTnSq6VDJPB+0NiHu2ZxSKEIxHN6uPAPnbXYNCe";
      mail = "lass@prism.r";
    };
    mors-repo-sync = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGv6N/UjFnX5vUicT9Sw0+3x4mR0760iaVWZ/JDtdV4h";
      mail = "lass@mors.r";
    };
    sokratess = {
    };
    wine-mors = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEKfTIKmbe1RjX1fjAn//08363zAsI0CijWnaYyAC842";
    };
    Mic92 = {
      pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE";
      mail = "joerg@higgsboson.tk";
    };
  };
}
