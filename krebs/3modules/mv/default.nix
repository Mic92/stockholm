{ lib, ... }:

with lib;

{
  hosts = {
    stro = {
      cores = 4;
      nets = {
        retiolum = {
          addrs4 = ["10.243.111.111"];
          addrs6 = ["42:0:0:0:0:0:111:111"];
          aliases = [
            "stro.retiolum"
            "cgit.stro.retiolum"
          ];
          tinc.pubkey = ''
            -----BEGIN RSA PUBLIC KEY-----
            MIIBCgKCAQEA0vIzLyoetOyi3R7qOh3gjSvUVjPEdqCvd0NEevDCIhhFy0nIbZ/b
            vnuk3EUeTb6e384J8fKB4agig0JeR3JjtDvtjy5g9Cdy2nrU71w8wqU0etmv2PTb
            FjbCFfeBXn0N3U7gXwjZGCvjAXa1a4jGb4R2iYBYGG3aY4reCN8B8Ah81h+S0oLg
            ZJJfaBmWM5vNRFEI5X4CLaVnwtsoZuXIjYStgNn/9Mg/Y6NQS0H0H+HFeyhigAqG
            oYGqNar/2QqPU176V/FwrD30F3qJV1uyzuPta7hmdfOxqYjZ/jqdPSRYtlunYYcq
            XbH5oYmzO9NEeVWzjdac/DiV2OP8HufoYwIDAQAB
            -----END RSA PUBLIC KEY-----
          '';
        };
      };
      ssh.privkey.path = <secrets/ssh.ed25519>;
      ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+7Qa51l0NSkBiaK2s8vQEoeObV3UPZyEzMxfUK/ZAO root@stro";
    };
  };
  users = {
    mv-stro = {
      mail = "mv@stro.retiolum";
      pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCxM34g1GUm5EtU00DAOlGSx8MsCWunhGTrozurj460QT7EdUbZvj0AcrQC0lP9kaZyhX+KueTjmLC+ICsnlHYeg4zoSEnSAUkccuyZxfgynVc4wrpfNAc1nHjDhDb/ulnC+8wNxvxUpI0XlBgu/Y7AbbChZj3ofv6uGGHJKfG3uSyCkt9VTCi1KwydHpe9P252N8NbopnbnkT0EMkRHruh7ICEKr4/ivmUL/IUrbFicEeCy4SeRAl8+00x4WqqvbBPzgdXn0AIjKLvus3dBoQubJNpUoXnyXJbElnit5a7QcgZJNLMbV0kf9zzCGduxkADzHkAFB9D4PuSMYt62iy12QlGbm80A9ncuwaSyJf7hPTvNbU8VyCblyfRz/SCaudUrfk5Xbxxu26FHi4hZqr3IUQt4T8pD8JWYGl4n2ZKnD8hHz/jrmNBK8h9d+VFafU9t1hRxlFsW1AhMEM+kfWClyhfTcKBKbml2a657lgUEVmlZt+18kwwsivM1QhHNTgxn5urRXRkh1VQ40UQroVuV1OUmvAngyAthF441VPGc5z7kEI+D4qjmUjSy6k4dvEy/RGfsAgJCf63zilRuUbL68f2OpxE8aeZZUXPvgdLml284pry7+C5sjlnCDoJfCj/yhdVx6mU9pWUd/Q97CLQewbsYhMzsqlBlIkXuipkDQ== mv@stro";
    };
  };
}
