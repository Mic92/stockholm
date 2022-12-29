{
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.36";
      aliases = [
        "bu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAxjAvT1sfHPWExhWRoXG+NJbYUmf5q4yfpfBRvb232LC9sLn4Z2wb
        hxKreR5/j9a/2hRIlCz4IwKftl5vroG9Vy4e7zZIz6QvN4TqED8dUjJ1ubhtj47l
        jjHW4cHLUWsaqqu6TAuPH26qPSxm9VrD6rZIX9RmQ1bWIaonVB3Q+XnDfPlISw6M
        gbQXz4tOsOnC+y/6C3VPUo0nqC+PuA/kyRq/ivVutKd0dTSY8LmCDNla6AEVD5dG
        sIqPWX5h8fjqU7G3oOMvMsBrCkvRRB0F0dQzGo8EXwCDJxa+xOuk5n1GYJ2lqeM/
        st7KIxmLvO5AE7cUxdLlDj4EzVLSDoAqOwIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "/MXEuv96HlrpHBto8KP2S6Ztiahhi3H7AevmbYS+xqE";
    };
  };
  secure = true;
  ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1Y13PvTn+9VjQbgy2ZmpAEFXyYaroYP/5nK9o7B8cidf01Sh39184mG8KN8VuEzCj7b37KnLH8qUDcsukvkxOVSoVHmXH+/Pgbmsxp4c9sxLQLHBfCazhT0S3Zs+BkR6LNQ8GOCS1qsgy05L6fMXoQgds3Zx/X4ZYjLnYVnJo8k+6aP4pU/rB6GFzGG9UrLDvSvk/PoswpEr7S6uFa4bF8JWD5VPkQTPTNwm1LWH4va+ABcw9KOgL2tsAk/jJlkLD4qgXowqgbwcpfe+QCukJb7uIQjRtOgxSAhHqT1nxjS6gROhGt0ojuwALaZaFPr9YtGlqxPhUzAXWKvvbVcr6kkR17HrtXZeLdFqwrUPlkIDFV6yLbYzQGKPFwxtpoJaH/irv6cgeXnHaa9XQJk+XJ5pE0X9uNljGr3B8LMKymdlvvBiWOOLpYsHg5aVOR+K7HvydLSuaah8hpCLjjVyIYIl/pIDL4F/FUSxcFBB4fgdXB77LXm5UizmI7+dqZaOQSm8qXbLZ8P+13ele2JyV1pmvJbLFlhCksDMOXx9jvSJQ6DOjPd+2vtABWh9XGo2Fiy+ekB9LTzlW+xON4FRZDoTPrPmhg40v+s7lySHx3miwCIJfNfLJpf0dxm3pQYWZPIra1RA9hbgstXBJ3+2VA5JEuVRt0SEygN5Kgk1Y5w== root@bu";
}
