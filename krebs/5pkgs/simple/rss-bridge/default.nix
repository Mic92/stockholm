{ config, lib, pkgs, fetchFromGitHub, stdenv, ... }:

stdenv.mkDerivation rec {
  pname = "rss-bridge";
  version = "unstable-2021-01-10";

  src = fetchFromGitHub {
    owner = "RSS-Bridge";
    repo = "rss-bridge";
    rev = "98352845a14b9f2eb8925ad7a04a5f6cc6a5af06";
    sha256 = "1nv1f6f17cn057k9mydd3a0bmj2xa5k410fdq7nhw5b7msyxy2qv";
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
