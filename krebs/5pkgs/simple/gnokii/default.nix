{ lib, stdenv, fetchurl, intltool, perl, gettext, libusb-compat-0_1, pkg-config, bluez
, readline, pcsclite, libical, gtk2, glib, libXpm
, fetchpatch
}:

stdenv.mkDerivation rec {
  pname = "gnokii";
  version = "0.6.31";

  src = fetchurl {
    sha256 = "0sjjhm40662bj6j0jh3sd25b8nww54nirpwamz618rg6pb5hjwm8";
    url = "https://www.gnokii.org/download/gnokii/${pname}-${version}.tar.gz";
  };
  patches = let
    base = "https://aur.archlinux.org/cgit/aur.git/plain";
  in [
    (fetchpatch { url = "${base}/gnokii-gcc7.patch?h=${pname}"; sha256 = "0q1f783mxx3h212y8kqazvcy3vv4rxbvf123wy1zpsyw2z9fwsa5"; })
    (fetchpatch { url = "${base}/gnokii-gcc5.patch?h=${pname}"; sha256 = "1vfsnhk37qzykx8sfilggwn1vq6iknx2r5z7rd4zrhgid2yqi2qc"; })
  ];
  buildInputs = [
    perl intltool gettext libusb-compat-0_1
    glib gtk2 pkg-config bluez readline
    libXpm pcsclite libical
  ];

  meta = {
    description = "Cellphone tool";
    homepage = "http://www.gnokii.org";
    maintainers = [ lib.maintainers.raskin ];
    platforms = lib.platforms.linux;
  };
}
