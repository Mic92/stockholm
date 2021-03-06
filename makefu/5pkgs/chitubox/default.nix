{ stdenv, autoPatchelfHook, libglvnd
, libgcrypt,zlib,glib,fontconfig,freetype,libdrm
, libxkbcommon
, libpulseaudio
, xlibs
, gst_all_1
, kerberos
, alsaLib
}:
# via https://raw.githubusercontent.com/simon-the-sourcerer-ab/chitubox/main/default.nix
stdenv.mkDerivation rec {
  pname = "chitubox";

  version = "1.8.1";

  src = builtins.fetchTarball {
    #url = "https://sac.chitubox.com/software/download.do?softwareId=17839&softwareVersionId=v${version}&fileName=CHITUBOX_V${version}.tar.gz";
    url = "https://archive.org/download/chitubox-v-1.8.1.tar/CHITUBOX_V${version}.tar.gz";
    sha256 = "08fh8w7s5qvlx6bhdg24g81a7zprq7n8m27w2vdv0cd8j0wixbsx";
  };
  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = with xlibs; [ stdenv.cc.cc.lib libglvnd libgcrypt zlib glib fontconfig freetype libdrm 
  libxkbcommon libpulseaudio kerberos alsaLib
  xcbutilwm xcbutilimage xcbutilrenderutil xcbutilkeysyms
  gst_all_1.gst-plugins-base gst_all_1.gstreamer
];

  buildPhase = ''
    mkdir -p bin
    mv CHITUBOX bin/chitubox

    # Remove unused stuff
    rm AppRun

    # Place resources where ChiTuBox can expect to find them
    mkdir ChiTuBox
    mv resource ChiTuBox/

    # Configure Qt paths
    cat << EOF > bin/qt.conf
      [Paths]
      Prefix = $out
      Plugins = plugins
      Imports = qml
      Qml2Imports = qml
    EOF
  '';

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  autoPatchelfIgnoreMissingDeps=true;

  meta = {
    description = "A Revolutionary Tool to Change 3D Printing Processes within One Click";
    homepage = "https://www.chitubox.com";
    license = {
      fullName = "ChiTuBox EULA";
      shortName = "ChiTuBox";
      url = "https://www.chitubox.com";
    };
  };
}

