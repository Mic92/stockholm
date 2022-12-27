{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.66";
      ip6.addr = r6 "12ee";
      aliases = [
        "green.r"
      ];
      tinc = {
        pubkey = ''
          -----BEGIN PUBLIC KEY-----
          MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAwpgFxMxWQ0Cp3I82bLWk
          uoDBjWqhM9Pgq6PJSpJjyNAgMkKJcQnWi0WpELaHISAVqjdPGUQSLiar++JN3YBx
          ZQGFiucG0ijVJKAUbQQDYbc+RGK8MGO2v3Bv/6E56UKjxtT1zjjvkyXpSC7FN477
          n9IfsvIzH/RLcAP5VnHBYqZ467UR4rqi7T7yWjrEgr+VirY9Opp9LM9YozlbRrlI
          hYshk5RET/EvOSwYlw/KJEMMmYHro74neZKIVKoXD3CSE66rncNmdFwD3ZXVxYn6
          m3Eob8ojWPW+CpAL2AurUyq4Igem9JVigZiyKGgaYsdkOWgkYLW2M0DXX+vCRcM6
          BvJgJn7s0PHkLvybEVveTolRWO+I/IG1LN8m0SvrVPXf5JYHB32nKYwVMLwi+BQ1
          pwo0USGByVRv2lWZfy3doKxow0ppilq4DwoT+iqVO4sK5YhPipBHSmCcaxlquHjy
          2k1eb0gYisp0LBjHlhTErXtt4RlrUqs/84RfgtIZYUowJfXbtEbyDmLIlESbY7qk
          UlXIMXtY0sWpDivWwpdMj9kJdKlS09QTMeLYz4fFGXMksFmLijx8RKDOYfNWL7oA
          udmEOHPzYzu/Ex8RfKJjD4GhWLDvDTcyXDG9vmuDNZGcPHANeg23sGhr5Hz37FRT
          3MVh92sFyMVYkJcL7SISk80CAwEAAQ==
          -----END PUBLIC KEY-----
        '';
        pubkey_ed25519 = "WfH8ULtWklOFK6htphdSSL46vHn6TkJIhsvK9fK+4+C";
      };
    };
    wiregrill = {
      ip6.addr = w6 "12ee";
      aliases = [
        "green.w"
      ];
      wireguard.pubkey = "lOORkStNJ6iP5ffqjHa/kWOxilJIMW4E6BEtNvNhLGk=";
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH0wqzo7rMkyw6gqTGuUp8aUA0vtwj0HuuaTIkkOnA30 ";
  syncthing.id = "CADHN7J-CWRCWTZ-3GZRLII-JBVZN4N-RGHDGDL-UTAJNYI-RZPHK55-7EYAWQM";
}
