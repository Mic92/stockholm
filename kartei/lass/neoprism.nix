{ r6, w6, ... }:
{
  nets = rec {
    internet = {
      ip4 = rec {
        addr = "95.217.192.59";
        prefix = "${addr}/32";
      };
      ip6 = rec {
        addr = "2a01:4f9:4a:4f1a::1";
        prefix = "${addr}/64";
      };
      aliases = [
        "neoprism.i"
      ];
      ssh.port = 45621;
    };
    retiolum = {
      ip4.addr = "10.243.0.99";
      ip6.addr = r6 "99";
      aliases = [
        "neoprism.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAwQiPQT9XQkeAIMohNhIVH1Er73LS36JQu/bokNSAlgRjiHfmWVQw
        hpmI0hO5ewI/HSxVH8MqITTjj8fp5+TOY5rxb3qj9SKGmoDpENw7g7BJsrpydu8+
        hdvC4btCibAeTeaNqubPMoJLnwuh7NJ9ucYAcRU24FI6qR/Q973a3rzWYBfPd4w9
        +Lq3ltFE4m6eLiL4ruQGR9Fc4HOJshJlUDUovGIC/98Fu468OuCaka4fR/IXD13O
        khc5LfAzm2PLuD25YZRjw27Pv3txYOWzb9ZfI8BS+7WUg1nKPDVZErvj97OouqVH
        binDgKLdLsamJgi+BrZs9uoxmXK9b459B3J6z4/d8dXTAW/cczqsODzsJnvw8IEE
        u45Pm3sY49vmnNsVhDEIPad3ZDitgeWW6UVBR+EJHp+r1TZ8eLaeUTdV6x3zIrHv
        dkobgI/0ynujSeMVzXA8cRDuLLVz0CwvNQ9FWzciZw4prOPjUDeSaOlIISOD4q8O
        u/jRfaIzPuQNyQN/0B9gUacHOGkQ3sZ33gFt1j6YdfjWnHn2Ddxm99nXfYUo82oC
        tEMui/7Vtj5G9dqDCzEacECvKqNVY2MRq5gpX+X5IwSbNc/vmykqhuDB5fzZWXRD
        AmRfNCsuFCw3EehPWkdH9JJxysBa52sAB387CL44bJ2rfRglTAKZYNUCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "/k2/hpq3XdSKfPPSAolfIx/AUgtKNF6kgv+WRTKtMqG";
    };
    wiregrill = {
      ip6.addr = w6 "99";
      aliases = [
        "neoprism.w"
      ];
      wireguard.pubkey = ''
        lhMJvEZOREjCSS3BbBxel0dJ3Mxjj0m82sUXqyYlUx0=
      '';
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEljpF/rqA2o9CcZny8Kdg1Ij9JmHsmuS/ii+HS5T7rW ";
}
