with import <stockholm/lib>;
self: super: {

  # https://github.com/proot-me/PRoot/issues/106
  proot = self.writeDashBin "proot" ''
    export PROOT_NO_SECCOMP=1
    exec ${super.proot}/bin/proot "$@"
  '';

}
