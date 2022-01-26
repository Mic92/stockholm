{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
{
  # https://github.com/Lassulus/CVE-2021-4034
  security.wrappers.pkexec.source = lib.mkForce (pkgs.writeText "pkexec" "");
}
