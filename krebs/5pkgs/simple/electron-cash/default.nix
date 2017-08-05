{ stdenv, fetchFromGitHub, python2Packages }:

python2Packages.buildPythonApplication rec {
  name = "electron-cash-${src.rev}";

  src = fetchFromGitHub {
    owner = "fyookball";
    repo = "electrum";
    rev = "a2245ea";
    sha256 = "1a0ym94azfd1yn97n2jcky344ajbj2amr9l6jpx30pqxndffpbgv";
  };

  propagatedBuildInputs = with python2Packages; [
    dns
    ecdsa
    jsonrpclib
    pbkdf2
    protobuf3_0
    pyaes
    pycrypto
    pyqt4
    pysocks
    qrcode
    requests
    tlslite

    # plugins
    keepkey
    trezor
  ];

  preBuild = ''
    sed -i 's,usr_share = .*,usr_share = "'$out'/share",g' setup.py
    pyrcc4 icons.qrc -o gui/qt/icons_rc.py
    # Recording the creation timestamps introduces indeterminism to the build
    sed -i '/Created: .*/d' gui/qt/icons_rc.py
  '';

  postInstall = ''
    # Despite setting usr_share above, these files are installed under
    # $out/nix ...
    mv $out/lib/python2.7/site-packages/nix/store"/"*/share $out
    rm -rf $out/lib/python2.7/site-packages/nix

    substituteInPlace $out/share/applications/electron.desktop \
      --replace "Exec=electrum %u" "Exec=$out/bin/electrum %u"
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/electrum help >/dev/null
  '';

  meta = with stdenv.lib; {
    description = "A lightweight Bitcoin wallet";
    longDescription = ''
      An easy-to-use Bitcoin client featuring wallets generated from
      mnemonic seeds (in addition to other, more advanced, wallet options)
      and the ability to perform transactions without downloading a copy
      of the blockchain.
    '';
    homepage = https://electrum.org/;
    license = licenses.mit;
  };
}
