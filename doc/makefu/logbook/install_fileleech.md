# install fileleech

```
builder$ python3 host.py --create-ssh-keys --create-passwords fileleech
iso$ fdisk /dev/sda # 3 partitions, grub,boot,crypt
iso$ cryptsetup luksFormat /dev/sda3 --cipher aes-xts-plain64 -s 512 -h sha512
iso$ cryptsetup luksAddKey /dev/sda3 hddkey
iso$ cryptsetup luksOpen --keyfile-size=4096  -d /dev/disk/by-id/usb-Intuix_DiskOnKey_09A07360336198F8-0:0 /dev/disk/by-id/ata-INTEL_SSDSA2M080G2GC_CVPO003402PB080BGN-part3 luksroot
iso$ mkfs.ext4 -Lnixboot /dev/sda2
iso$ mkfs.ext4 -Lroot /dev/mapper/luksroot
iso$ echo 1 > /proc/sys/net/ipv6/conf/enp8s0f0/disable_ipv6
iso$ mount /dev/mapper/luksroot /mnt
iso$ mkdir /mnt/boot
iso$ mount /dev/sda2 /mnt/boot
iso$ mkdir -p /mnt/var/src
iso$ touch /mnt/var/src/.populate
```
