_:
{
  imports = [ ./CAC.nix ];
  nix = {
    buildCores = 2;
    maxJobs = 2;
  };
}
