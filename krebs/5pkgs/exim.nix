diff --git a/pkgs/servers/mail/exim/default.nix b/pkgs/servers/mail/exim/default.nix
index 0918e30..5b7a587 100644
--- a/pkgs/servers/mail/exim/default.nix
+++ b/pkgs/servers/mail/exim/default.nix
@@ -1,11 +1,11 @@
 { coreutils, fetchurl, db, openssl, pcre, perl, pkgconfig, stdenv }:
 
 stdenv.mkDerivation rec {
-  name = "exim-4.87";
+  name = "exim-4.88";
 
   src = fetchurl {
-    url = "http://mirror.switch.ch/ftp/mirror/exim/exim/exim4/${name}.tar.bz2";
-    sha256 = "1jbxn13shq90kpn0s73qpjnx5xm8jrpwhcwwgqw5s6sdzw6iwsbl";
+    url = "ftp://ftp.exim.org/pub/exim/exim4/${name}.tar.bz2";
+    sha256 = "0bca3wb45hl7h8m8bpvsmrmqa07jhbhqyigs9pl29hhzwgbmz78i";
   };
 
   buildInputs = [ coreutils db openssl pcre perl pkgconfig ];
