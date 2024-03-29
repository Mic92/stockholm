{ r6, w6, ... }:
{
  nets = {
    retiolum = {
      ip4.addr = "10.243.0.15";
      ip6.addr = r6 "012a";
      aliases = [
        "orange.r"
        "cgit.orange.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAlnHedIf4f3/6Wfl5PSSz+7KvdIMkygp5m/U270sdPBh46MqYa8cn
        OfPq40LcbWIZqAVex7mP+fK7vq8LTIr+sCKvzY46o3ZLbQQ7cCtQi02GFnSAPhVT
        4XEmPn9dX/nRmI8xQqzh5jRMpgeOKE+xY6QfgkERD9mflkJi5dGYCOVW1UUK7pHR
        7giCrUiLuQbUeIz+G7KOeIRHxU8dwD8it1Jk6KxdM3MW6HwFsuqZu0qjbBPKhTEe
        fgzSTDtZEGmcQw5vA/RwjxoRvKYThbK/lLoVJItFAhUCWUJA8bJuIanwzPfOF0JO
        xWkxiY3ntvn5ykbvhF6LoHE+kEfcBJzBfRFRSXV5qU5wW1FC4AQylUDrest/qXQh
        DY8boUqK/hi/MlC2ciPH+DlBOi5wduWty8F0KqNzjg1IIEOk8H+z9hgBDbdJnYHH
        MBjYOZ3MFpoNb2VCJTE7dlIarVdH1OOO2KkzX/GGW7wGQK94iqLHjBcGl15GcGOz
        EOivq+783VOtzZGS4jd8D0OcCo725FzhuWi6KR5QTljwrd5C1gGFoAW7RCsUiveZ
        0by9aB+G2DWmSRWZsmPnnbYo6yPvp+WR2yfPu1pKwjyNsmAgTYm4bkwRIvODb6Xk
        ShgawP5V8RDp+hUmr27KgJvUJnQbVeJf9SO1pT7IfNOjLwHv26iOo7UCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      tinc.pubkey_ed25519 = "dVIOgHjuKLDJ+QB+sDjL9Pk3pXs8wKo+gemGvNG3z1H";
    };
    wiregrill = {
      ip6.addr = w6 "012a";
      aliases = [
        "orange.w"
      ];
      wireguard.pubkey = ''
        NP8zM9+ocwsHhY9Rn6tFqIU1FR8JidqtDs7IKpl3yU8=
      '';
    };
  };
  ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDnHnTPPwMW1Oy3DBuaT4fG5ryhWmVS9Y8Sw0ezUGuLn";
}
