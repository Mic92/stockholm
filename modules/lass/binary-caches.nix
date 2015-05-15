{ config, ... }:

{
  nix.sshServe.enable = true;
  nix.sshServe.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBF9SBNKE3Pw/ALwTfzpzs+j6Rpaf0kUy6FiPMmgNNNt root@mors"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFCZSq5oLrokkh3F+MOdK5/nzVIEDvqyvfzLMNWmzsYD root@uriel"
  ];
  nix.binaryCaches = [
    "scp://nix-ssh@mors"
    "scp://nix-ssh@uriel"
  ];
}
