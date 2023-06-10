{ config, lib, ... }: let
  inherit (lib) flip mapAttrs optionalAttrs recursiveUpdate;
  slib = import ../../lib/pure.nix { inherit lib; };
  hostDefaults = hostName: host: flip recursiveUpdate host ({
    ci = false;
    external = true;
    monitoring = false;
  } // optionalAttrs (host.nets?retiolum) {
    nets.retiolum.ip6.addr =
      (slib.krebs.genipv6 "retiolum" "external" { inherit hostName; }).address;
  });
in {
  users = {
    "0x4A6F" = {
      mail = "0x4A6F@shackspace.de";
      pubkey = builtins.readFile ./ssh/0x4A6F.pub;
    };
  };
  hosts = mapAttrs hostDefaults {
    crustacea = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "crustacea.r" ];
          ip4.addr = "10.243.42.63";
          ip6.addr = "42:0:4a6f::4263";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA1dA67Uq6IcWTWVVcg5kO4OUcmYY/mUzERK6WwrU7m+Qq2ovA2Fh1
            VTxfNzJg8zgyrBbUwpaLE4LuRgyrYbPABwgNMXS6wnHdunbm0x5RUcih/IRNobV1
            uf2Q/rVcrXHZD5+YL09hTZnU7PVkZm6WX0fc79rEKYIEopPpomCs2mECPSmqZPaW
            L9wprtRTuQ3V0xxrCuUKX7SxANEursM8SvNfKydWdaUxjIV2iGVKuVUkAZHwx5jJ
            roKEriWsIJ6FHTMb1v5yWXrvngMgYlKrpF6/m/AHEkQoPsEJ+oBkn0fop9pfyZjM
            WzMhZHcKFYebSI4HqFRuQOc1scIzUdpC/sZYLYlddbwpJHj4xdJwIUN03Uga/KRQ
            n1SrJnhmXonHvJZFBYcNDR6aTtdN7mJVBv8bQ7DGt1q6Gp8QItQqvpdzq314+Pw6
            0EVKPaqdz6Cqpwn8RtJ9ZGb6BE3yUrpJkU25DyCSO86LmeCchApwssghWvPsbBDg
            iF4QCyrWJ2HFnl7jJDGbEajHaE/xko2dt1F5frTWxsmDHRKSRhaGDwp5qgFUpCa0
            2h+zZqkG4boV6CrMEjStb15EOXTUVfq0DPojFik6agCltslsJAwp+f1fb7NSee4d
            TNWb1CHfIQWLPnm1LFwphSqyHY/9ehcsX3PJ7oXI+/BnV8ivvoApWA0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "DWfh6H8Qco+GURdVRhKhLBAsN5epsEYhOM2+88dTdTE";
        };
      };
    };
    mystacocarida = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "mystacocarida.crustacea.r" ];
          ip4.addr = "10.243.42.64";
          ip6.addr = "42:0:4a6f::4264";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAs3B22OzRE7kPInW901npOyRlIuk/vMb834a2BEKSb6+taXqf5rXm
            r4isRaZsoQACNS8b/vpvQGDITyA4Ji4S0WpbpTklYdx0VxLM33ezuvxeR4ZyPtrW
            le47/kgJd2E/0e5QuU10P0i7VuauFdG6y9Iajts0pz5h7XJNU1pMoww3MF8+4iJ/
            KvZjWHy6d+5gQ888do7vxbuAhaGK42KE+i9g0v5xtPzdFmJ9YdR8E2Ru3jH1DzWC
            UdsOkqKmlBVaLSwmKIZ4c5JgSF/UuQxxJ0Yb/eZzPdltc1fa9aB/us3oFNhIsU0I
            nJ0nW8LYferV9dl8w2v8Utvy6mPKObz0cBmFbXBIPMIrnVuJOpkulEei8W31m2cS
            RcQOMFRBHvH4TugRvgbAwylrPH+aQ9+49g1RIMFyWBF/8Rt5H3ncFuWUvv8SQneQ
            hJMOrEX5E4cfZMPvGNXNjinzngUzfh/QKaNxRBo0bXF4kyuZFcZ4Q7H9TiYchuwn
            s5ZoaTmQ6WU0OpyUC6aUyKhB34+nJF/ySvzPnfpecAjN7NHXWgBzqLgCmHskrtgU
            ukQ6Yy/8tF1hbrRrN3p+ZXk+9Fmqa4dD517UX6B2XXh5h7fOHCzQyOW+6mRHzbdB
            wclhjSKW1+HxV9T1q1RP162v/CD1ozjDuXvLX6awPZ+AQf6lBAG/NbECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "6owOO5CReYHueN4v2joTy31NzrqLHe858TOvzeg0G+G";
        };
      };
    };
    tantulocarida = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "tantulocarida.crustacea.r" ];
          ip4.addr = "10.243.42.65";
          ip6.addr = "42:0:4a6f::4265";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA45SZZbq5HBG98oY8p3ZJFSfoMwopfLHMQ0fFvn7O0FOOPsTomsD1
            S61HTSsGqHJ35TfNV+lh4jrldN1wPkSl8yhD6+SKoiVlM+UqJ2HQLQhFL6ZoOlOe
            YSZ2Ws5ydnP32G61oV/1nfIp/q/jzrT0yWepol+HJth2YSHkDuBU1zA4ESkNhnj8
            o9Qnryxq6PjLqqWq6y+grcR8eer/IlDTTJn0Mb6lk6gDivan2k0ryzSwWCZt5B9o
            s10oIJqS1LxyrS35MP/vEI/FgcA1SX0TaVjMKbu4RwBqIXXC6Mb36PjRPbBf2P5J
            5ZMOf1uwesnlUsWgmzEVFUnflqHTpRb/2kjDngvP8Ed7Q/tN0csWPl8mj8ghpwr1
            +d7utmpjgW+glVPpFVGniGq5DVkXYBU6Wfg84weyyDDYP65R/81fRgesx14ikJqI
            ML89G9hd/FiIjNk3sW3v5gj1FNl0R/sgsDjTnmvgTr3t8bBgsB2SSFa9VxgHToO0
            rfRPYlAtN9YwIizgUImB6fhqMNzXRJnZsDcMqUmaAaKa1x6wwSR/QQlds9sPJDB+
            ggkEaKpYsAzwZ7vjOFDdqH40zqCGM767UoPliThSfn4DPQnTSHzSyuR2dS8n26/r
            wXTwb2yDDO6pNzHqgXS+whB2PCUrhfJUKfW60H3Lyn6umxjZ2rFspI0CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "eoyCx4PvqsFpsyJq5X8J7zb+2oytliv0w3MIDIiaSTI";
        };
      };
    };
    thecostraca = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "thecostraca.crustacea.r" ];
          ip4.addr = "10.243.42.66";
          ip6.addr = "42:0:4a6f::4266";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAoLm9vVoUg79egwX5KDYdJLJvEygz5bh7r0/BcCrGeWUBn+S8HzPY
            aVBEW/rcLK7blksGhnqo6HfncKlYH43AUzt3HTcjlfhMCeC4VLn+0f+QiW/HV4H4
            k3IC9S8Imo7t5eDqHjchPqTyh2AuilbDHM6VoIgD95ozR8dtvWy9NL7dIOAxyrVF
            m3WdHg5i7G+xus5u/Q35rCSCxpSsmkUBfEjixSC9lpQdMfc5Vvqy8qD/unLVNs1G
            nFoFTtxrz9EVRz9fmYx/mFMC9FHuUD6qmG5YNjS5wWcOGw04GGRnwwT9rnuI3NOD
            ttYk3b7cn3V+jOD8zCtcyH2DSIcVBIFbTULmX1Xq1462/IiyH0nQNH7DSbKGDc/V
            OHYZOfiy5Wotua5BqLxZ4W3b0QDLjkLc06pD6YvOhZbs4igl4O+zDCCyE4OakEzk
            1qugSumGM/hEavJFR96CWgyiJQOV1tnNWs7QDwtdZRkGaC7sPInc56RvHWjP0jDH
            tcnfSDPPLdpf9Kl5VhmXlOWvg0FCuxLVlCcG8yJDx/Xm26GU6t+avi14cl29hX0M
            lQF7x/sVpdTQtsv3IZ1glt/vhS/egucSHKXJ3VcRWlQkQagqDqwINYyaS5xoWmEk
            4m6L/ndWc/y1A3zFC/4STsEyx4QuQcpMigkcz+42Ubb/wtQZGEmBND8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "Bfa9di1XiTzGTuDo+L7spfouq1WUc1pvQH9mziHVLEB";
        };
      };
    };
    branchiopoda = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "branchiopoda.crustacea.r" ];
          ip4.addr = "10.243.42.67";
          ip6.addr = "42:0:4a6f::4267";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEAtNKC/MIY3X1vYR2tRz4jXEmqRFmUdQfwb3Fl55Bw/5GVySDe7Rtm
            8+MyWRp18D9DVDNJ/mvhX4nYA5OR2q1m5VPX+jdkPbD+9KYVWWQpnPkKIdon1FpO
            sw52BMx/jMdhq+6h510LSvFrVP4jnPaETzEMyLGdKqqq4R7D/KrkInLBtyAisjmN
            6eeBZAfr+INrkpUY7zhArtzfDbTXqCLFR3ufxlBzVL0of/oDGw1Zvc5TOiAtWmOa
            iVT/jzTqqf9Yhs1ot3Bxzf0pOrWNpGF6S3H8fl4kX/hGMVowjvKW7cLou38W5dOS
            uweS6a/5dT3Z9RIQvnwjTMJ2/WjiRL3Kivk22W+58eqBxBAE1Fs0ujwGlGLYLATF
            RSoD9N15agfiycqzy044Hg7CCBdyFcJF3aTWZ/59XmMIav3Liv52lmE/KdQyTnsL
            p6dBjuyPGnrfrBZv/WDWEmWEkSFkh2oKCJYysIDxh1XfxxczacD4UTEjUce0ehbn
            rTjkO5E2n5rMLYzsnC2XwolmXiHNdvhp4rl4lv+GzmJhsJFtJjx13WtI0XjnyPd9
            WfJs7HHR8mK7FC0J2f1Rqsji9lxHVRMCu27jAaEuCi3Gsk33+UpgR2XvO9oxkMRz
            RVIDXMl6NXy/nLaUmNKp8sPMfXycikY89ur9Z2OcgQ9/Q4tCPTS6GQ8CAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "Y/1hm0pGCElydpV23tQy5ibTBTNgs1fqHl4qPARAyRC";
        };
      };
    };
    copepoda = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "copepoda.crustacea.r" ];
          ip4.addr = "10.243.42.68";
          ip6.addr = "42:0:4a6f::4268";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA9eLYq5DVkWeFiqXz+OPENj5VljBoOOBDSwRlD/+aABFGNd/g+gSh
            l3BUzkGNeH9gY8fHTSE4GzhkPRlK5+pRvx1xE3jWOT1iU+VIP8IKXGOIithZgWfg
            T2aEejOvhB+qn2Be6T6PZkTa8hLJorYRNbe+npZTll8iW0h7f3FZNKGSGYIfeRYI
            WHjtl0onv2lIGHi3fucUq2QTTd1G4GX7R2nI5XNvYLruSVj0dK2OU3oR4zNGWmlJ
            QryM1QY6l3trFmdAxLQeTtNWcgUSKQozlCg6yTVCB3roFzdQe+P6ltpj3XM3LPKy
            Vijk/tRvPozRZ87xVbuVcIVRYkvFewoAWmi8hITBxUTl4OG8qvXp8QAig/+ZpZN3
            dYPqFmv0zdPp+Jd77Qz72f2ZFjKU1fvL8dyh1Uyc2PZRgnWEWrxNXlswybMPKrmX
            rIEV3OMnqCcdI5JJFslUxiV5qGCcWTNF8yt0+OLe9gxS8G41hcxkENW50QgoZHon
            h4pJsrb7/7d4ReFFit6K/S01V3lOOtFIDxHCwwnG7gLyc4bIXZmAu85JxSisPUt0
            QJAxbSSKt9jduSpoqwXHXRtQdsPmFeClE9kVd3PAWHEFmqLLOdjvMxVVdVhGUabK
            lL1kGoidqPMU3IXLjsxfHb/rVtGWi9yXSUM3a0vJt6+bNVt4bGvrtTkCAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "HWfJMJfiiNXBywI0hvSbiep1/O9VpPu0T6cc/mU6dOI";
        };
      };
    };
    malacostraca = {
      owner = config.krebs.users."0x4A6F";
      nets = {
        retiolum = {
          aliases = [ "malacostraca.crustacea.r" ];
          ip4.addr = "10.243.42.69";
          ip6.addr = "42:0:4a6f::4269";
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIICCgKCAgEA0yIBnzkM8cJDEC5d/J8Pj/wT27PAMu7r1DLc9O1RkgD24tPhxq1k
            W8Eo/NnirjcLjoJLG4V/GFRdVwLNEpngmRTGaqHknOZzGcFYUDLbrKGYULTjXheP
            a3fU+ZNlH9YVurzgXNA5suNXzQAp0eY4DqydYvdk1o6HSrbo0uyiEP48tx38rAS0
            N+V+kiQidtT45o+KrbEAAT/4znlsGLyB8u7U54Alrtukf86z70j3R2eUOleP2MDi
            5UPdCnoKZKDLQWs23vXeMCcpnLqUXyLdVPrDmZKq9tGLgxsK2CC9Xu3brjAV3wLp
            MeN1U55lxti+bkInFBMCjO8xS3clJ0W5DEtDV5QYCi78Niu99pe4KgRfDEA4PqK+
            aykv1ljpI43NlQBtJKsqXUtzDEpYs5zSmy/tqLxeXqmvSYd3/+5JRWzLvQZ4YxlJ
            EwjB6fxsBTe1eftohD2LiRWsjmvf0JWlb6+rfBoxMtX+wfI0yDR8Ozm8QRk9WgHO
            aUP28fzqoFCim7ti+QwT5EqwGcARh6jyGXmycx9swI4b/hfCAe39KsPNQ6jdeXYR
            1J9mmIM88ythaWypwieOqbCAul4pJosTFjMRiExnV7zvjFn/csVyqt3FkoJFrqmM
            37AxGyDKRLFV9aN7CNoaAfcNkaOhFNzPB9Q7PoXlPspdxvrATv3WU6ECAwEAAQ==
            -----END RSA PUBLIC KEY-----
          '';
          tinc.pubkey_ed25519 = "BEtuMBeKGXYWODlDIwlGU1eGZ7OqryxgDqnx1cJX8ZE";
        };
      };
    };
  };
}
