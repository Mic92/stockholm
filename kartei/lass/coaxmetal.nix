{ r6, w6, ... }:
{
  cores = 16;
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.17";
      ip6.addr = r6 "17";
      aliases = [
        "coaxmetal.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAwcuMl/W6DZ7UMK4RHrxA
          xCc8CkqpUTYldPdB9KJmcH6OpbQqCcPxGOvRe42NdOfCyy11WjAjUMRGnzMyi4MK
          gMEjcrl5CnQd9nF9f8Mom8cuSOVm1j46qY7Trl/MsEKsKHiYAHtLFpHz2+UI+HBU
          WbSeDLLA8g79SZq/pqWHfp3YKzqP4p+dmi8j+aOZJWkGu9l+Q40qQrTJQCxYgEek
          ODeBFCY3DGfJRn79IFGuhF1/jGiAwF3/1j2Rxlesazl6/Lyvmtioplsqn8J94z32
          G5wyGpqn/BcXkJTlWtwb3Rrg6OOALJAqy2H5EoIVT26gwmvkEStMtvgLfAeYjL8F
          G2bAtaeQGzwQZNuVJAMI9Qtb+PHw322Wz+P8U669C/HCdGCumMf+M7UDHP79kXOO
          IFs1NvkU3z/iO/5bj41v8u0W8+b9NWe++dI8N8q0hWLPgnz5PI998xW06Dul7pAX
          K1OMIMfTTGgAZHAF1Kdn1BSXezgwkutwzy5h8XkYclyHB2nPXkXIYmahi1XgWeAE
          7B4NmefbS6H8dLOU7yMEWuxmYl41UOybtyrsp1za5wtERpQgzl6EWfIXISEdx1Ly
          bmb3SGtB85RyqqCe2O9DzVZCw7mXgN69R5efyEuq3HIIN9udLNrybPNNyD/OlAqo
          l/xwDxiSCEsO6yY5lGc0MCMCAwEAAQ==
          -----END PUBLIC KEY-----
        '';
        pubkey_ed25519 = "bEGgA5Wupw+Dgh6Ub7V21Y3wOmyspW1rKGrZsVhi3cO";
      };
    };
    wiregrill = {
      ip6.addr = w6 "17";
      aliases = [
        "coaxmetal.w"
      ];
      wireguard.pubkey = ''
        lkjR14oOVKl03/0sUzOmddf28ps+v5qRxrbRY03Pg38=
      '';
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO9vAYuTv07c9bOjDJId3ShXJ1qIEuyrjkVYkJn9yMET ";
  syncthing.id = "W5BJ4TL-GAQ46WS-ZB72HFS-XOURLBA-RNBVMYC-POFH4UA-CBORQID-BMIHNQZ";
}
