{ config, lib, pkgs, fetchFromGitHub, stdenv, ... }:

stdenv.mkDerivation rec {
  pname = "rss-bridge";
  version = "unstable-2021-04-20";

  src = fetchFromGitHub {
    owner = "RSS-Bridge";
    repo = "rss-bridge";
    rev = "716f5ddc0e20c10cb77ded46380cc376913a92fd";
    sha256 = "17aqmj7rz0ysk8nj4kbjvnsjdm47d0xsypfygzzk2vagxfz5w3p8";
  };

  patchPhase = ''
    substituteInPlace lib/rssbridge.php \
      --replace "define('PATH_CACHE', PATH_ROOT . 'cache/');" "define('PATH_CACHE', getenv('RSSBRIDGE_DATA') . '/cache/');" \
      --replace "define('FILE_CONFIG', PATH_ROOT . 'config.ini.php');" "define('FILE_CONFIG', getenv('RSSBRIDGE_DATA') . '/config.ini.php');" \
      --replace "define('WHITELIST', PATH_ROOT . 'whitelist.txt');" "define('WHITELIST', getenv('RSSBRIDGE_DATA') . '/whitelist.txt');"
  '';

  installPhase = ''
    mkdir $out/
    cp -R ./* $out
  '';

  meta = with lib; {
    description = "The RSS feed for websites missing it";
    homepage = "https://github.com/RSS-Bridge/rss-bridge";
    license = licenses.unlicense;
    maintainers = with maintainers; [ dawidsowa ];
    platforms = platforms.all;
  };
}
