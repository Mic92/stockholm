# transfer gum to new hosts

```
builder$ vim krebs/3modules/makefu/default.nix
## update ip
builder$ vim makefu/1systems/gum.nix
## update hardware config

old-gum$ rsync --progress -lprtvzF  . <newip>:/mnt/

new-gum$ touch /mnt/var/src/.populate
new-gum$ gdisk /dev/sda r;g;w # gpt to mbr

builder$ make -C ~/stockholm system=gum target=vcygfnhdxyxr47zu.onion install

```
