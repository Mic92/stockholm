self: super:

self.lib.overrideDerivation super.dhcpcd (old: {
  configureFlags = old.configureFlags ++ [
    "--dbdir=/var/lib/dhcpcd"
  ];
})
