From 2015-07-28 until 2023-07-28 here lived the configuration of makefu.

# New Location
All configutation can now be found at [Github: makefu/nixos-config](
https://github.com/makefu/nixos-config ) or [cgit: nixos-config](
https://cgit.euer.krebsco.de/nixos-config ) respectively.

# Background
With nix flakes it became possible to finally split the configuration up
into smaller chunks.

With the long hours **lassulus** worked at the GPN'23 to make stockholm into a
flake **makefu** used the opportunity to also flakify this configuration and
use stockholm only as a flake input.

With this the configuration became more pure (no nix-path includes), inputs are
described as flake inputs and locked as such and secrets are managed by
[sops-nix]( https://github.com/Mic92/sops-nix ). Deployment is now done with
`nixos-rebuild` only.
