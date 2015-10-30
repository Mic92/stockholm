_:
{
  imports = [ ./CAC.nix ];
  nix = {
    buildCores = 1;
    maxJobs = 1;
  };
}
