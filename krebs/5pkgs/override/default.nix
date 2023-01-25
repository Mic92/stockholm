self: super: {

  bitlbee-facebook = super.bitlbee-facebook.overrideAttrs (old: {
    src = self.fetchFromGitHub {
      owner = "bitlbee";
      repo = "bitlbee-facebook";
      rev = "49ea312d98b0578b9b2c1ff759e2cfa820a41f4d";
      sha256 = "0zg1p9pyfsdbfqac2qmyzcr6zjibwdn2907qgc808gljfx8bfnmk";
    };
  });

  # https://github.com/proot-me/PRoot/issues/106
  proot = self.writeDashBin "proot" ''
    export PROOT_NO_SECCOMP=1
    exec ${super.proot}/bin/proot "$@"
  '';

}
