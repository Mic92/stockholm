{ ... }: with import <stockholm/lib>;
{
  users.extraUsers =
    mapAttrs (_: h: { hashedPassword = h; })
             (import <secrets/hashedPasswords.nix>);
}
