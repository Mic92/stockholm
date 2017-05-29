with import <stockholm/lib>;

self: super:

{
  test = {
    infest-cac-centos7 = self.callPackage ./infest-cac-centos7 {};
  };
}
