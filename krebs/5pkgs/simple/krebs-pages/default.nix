{ bling, runCommand, ... }:

runCommand "krebs-pages-0" {} ''
  mkdir $out
  cp ${./fixtures}/* $out/
  ln -s ${bling}/krebs-v2.ico $out/favicon.ico
  ln -s ${bling}/krebs-v2.png $out/
''
