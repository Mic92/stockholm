{ python3Packages
, lib
, fetchFromGitHub
}:

with python3Packages;

let
  scriptPython = python.withPackages (ps: with ps; [
    aiohttp
    requests
    python_magic
  ]);

  version = "lassulus-fork";
in python3Packages.buildPythonPackage {
  pname = "weechat-matrix";
  inherit version;

  src = fetchFromGitHub {
    owner = "poljar";
    repo = "weechat-matrix";
    rev = version;
    hash = "sha256-o4kgneszVLENG167nWnk2FxM+PsMzi+PSyMUMIktZcc=";
  };
  # src = ./weechat-matrix;

  propagatedBuildInputs = [
    pyopenssl
    webcolors
    future
    atomicwrites
    attrs
    Logbook
    pygments
    matrix-nio
    aiohttp
    requests
  ];

  passthru.scripts = [ "matrix.py" ];

  dontBuild = true;
  doCheck = false;

  format = "other";

  installPhase = ''
    mkdir -p $out/share $out/bin
    cp main.py $out/share/matrix.py

    cp contrib/matrix_upload.py $out/bin/matrix_upload
    cp contrib/matrix_decrypt.py $out/bin/matrix_decrypt
    cp contrib/matrix_sso_helper.py $out/bin/matrix_sso_helper
    substituteInPlace $out/bin/matrix_upload \
      --replace '/usr/bin/env -S python3' '${scriptPython}/bin/python'
    substituteInPlace $out/bin/matrix_sso_helper \
      --replace '/usr/bin/env -S python3' '${scriptPython}/bin/python'
    substituteInPlace $out/bin/matrix_decrypt \
      --replace '/usr/bin/env python3' '${scriptPython}/bin/python'

    mkdir -p $out/${python.sitePackages}
    cp -r matrix $out/${python.sitePackages}/matrix
  '';

  dontPatchShebangs = true;
  postFixup = ''
    addToSearchPath program_PYTHONPATH $out/${python.sitePackages}
    patchPythonScript $out/share/matrix.py
    substituteInPlace $out/${python.sitePackages}/matrix/server.py --replace \"matrix_sso_helper\" \"$out/bin/matrix_sso_helper\"
  '';

  meta = with lib; {
    description = "A Python plugin for Weechat that lets Weechat communicate over the Matrix protocol";
    homepage = "https://github.com/poljar/weechat-matrix";
    license = licenses.isc;
    platforms = platforms.unix;
    maintainers = with maintainers; [ tilpner emily ];
  };
}
