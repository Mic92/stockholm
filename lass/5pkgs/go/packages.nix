{ self, fetchurl, fetchgit ? null, lib }:

{
  by-spec."formidable"."*" =
    self.by-version."formidable"."1.0.17";
  by-version."formidable"."1.0.17" = self.buildNodePackage {
    name = "formidable-1.0.17";
    version = "1.0.17";
    bin = false;
    src = fetchurl {
      url = "http://registry.npmjs.org/formidable/-/formidable-1.0.17.tgz";
      name = "formidable-1.0.17.tgz";
      sha1 = "ef5491490f9433b705faa77249c99029ae348559";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "formidable" = self.by-version."formidable"."1.0.17";
  by-spec."redis"."*" =
    self.by-version."redis"."2.1.0";
  by-version."redis"."2.1.0" = self.buildNodePackage {
    name = "redis-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "http://registry.npmjs.org/redis/-/redis-2.1.0.tgz";
      name = "redis-2.1.0.tgz";
      sha1 = "38acb208f90750250f9451219b73ff08ae907f94";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "redis" = self.by-version."redis"."2.1.0";
}
