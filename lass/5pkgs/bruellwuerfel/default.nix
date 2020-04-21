{ yarn2nix-moretea, fetchFromGitHub, nodePackages, nodejs }: let
  #src = ~/src/bruellwuerfel;
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "bruellwuerfel";
    rev = "57e20e630f732ce4e15b495ec5f9bf72a121b959";
    sha256 = "08zwwl24sq21r497a03lqpy2x10az8frrsh6d38xm92snd1yf85b";
  };

in yarn2nix-moretea.mkYarnModules rec {
  pname = "bruellwuerfel";
  version = "1.0";
  name = "${pname}-${version}";
  packageJSON = "${src}/package.json";
  yarnLock = "${src}/yarn.lock";
  postBuild = ''
    cp -r ${src}/{src,tsconfig.json} $out/
    cd $out
    ${nodePackages.typescript}/bin/tsc || :
    mkdir -p $out/bin
    echo '#!/bin/sh' > $out/bin/bruellwuerfel
    echo "export NODE_PATH=$out/dist" >> $out/bin/bruellwuerfel
    echo "${nodejs}/bin/node $out/dist/index.js" >> $out/bin/bruellwuerfel
    chmod +x $out/bin/bruellwuerfel
  '';
}
