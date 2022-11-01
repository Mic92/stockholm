{ config, lib, pkgs, ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      exim =
        super.exim.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ self.gnutls ];
          preBuild = /* sh */ ''
            ${old.preBuild}
            sed -Ei '
              s:^USE_OPENSSL=.*:# &:
              s:^# (USE_GNUTLS)=.*:\1=yes:
              s:^# (USE_GNUTLS_PC=.*):\1:
            ' Local/Makefile
          '';
        });
    })
  ];
  services.nginx.package = lib.mkDefault (pkgs.nginxStable.override { openssl = pkgs.libressl; });
}
