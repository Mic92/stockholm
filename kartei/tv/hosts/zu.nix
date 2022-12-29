{
  ci = true;
  nets = {
    retiolum = {
      ip4.addr = "10.243.13.40";
      aliases = [
        "zu.r"
      ];
      tinc.pubkey = ''
        -----BEGIN RSA PUBLIC KEY-----
        MIIBCgKCAQEAti6y+Qkz80oay6H2+ANROWdH4aJS54ST8VhFxRB3WdnlDFG/9t6d
        idU87uxW5Xmfm6nvpO0OPhG4E3+UI7KtWP71nnducpLV6gfob4f2xNGVG435CJ6u
        BgorbneUbJEfr4Bb0xd46X2BtLqi5/vUY3M5KMGE2sMdyL2/7oujEI8zQJCse95a
        OhDZdF2bCDEixCHahNprkQrD8t1lNYoLR2qtDZ5psIh5vgdp0WOOMGvUkCDkNjWj
        /NKaRXPhUVRDLRFEzMZhtFtSHzaofzrhGFoU1rGZwc/XopqpiFi0D7L++TiNqKAk
        b9cXwDAI50f8dJagPYtIupjN5bmo+QhXcQIDAQAB
        -----END RSA PUBLIC KEY-----
      '';
    };
  };
  secure = true;
  ssh.pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDNjHxyUC7afNGSwfwBfQizmDnHTNLWDRHE8SY9W4oiw2lPhCFGTN8Jz84CKtnABbZhbNY1E8T58emF2h45WzDg/OGi8DPAk4VsXSkIhyvAto+nkTy2L4atjqfvXDvqxTDC9sui+t8p5OqOK+sghe4kiy+Vx1jhnjSnkQsx9Kocu24BYTkNqYxG7uwOz6t262XYNwMn13Y2K/yygDR3Uw3wTnEjpaYnObRxxJS3iTECDzgixiQ6ewXwYNggpzO/+EfW1BTz5vmuEVf4GbQ9iEc7IsVXHhR+N0boCscvSgae9KW9MBun0A2veRFXNkkfBEMfzelz+S63oeVfelkBq6N5aLsHYYGC4VQjimScelHYVwxR7O4fV+NttJaFF7H06FJeFzPt3NYZeoPKealD5y2Muh1UnewpmkMgza9hQ9EmI4/G1fMowqeMq0U6Hu0QMDUAagyalizN97AfsllY2cs0qLNg7+zHMPwc5RgLzs73oPUsF3umz0O42I5p5733vveUlWi5IZeI8CA1ZKdpwyMXXNhIOHs8u+yGsOLfSy3RgjVKp2GjN4lfnFd0LI+p7iEsEWDRkIAvGCOFepsebyVpBjGP+Kqs10bPGpk5dMcyn9iBJejoz9ka+H9+JAG04LnXwt6Rf1CRV3VRCRX1ayZEjRv9czV7U9ZpuFQcIlVRJQ== root@zu";
}
