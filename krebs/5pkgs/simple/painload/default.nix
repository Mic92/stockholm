{ pkgs }:

pkgs.fetchFromGitHub {
  owner = "krebs";
  repo = "painload";
  rev = "a963b45d5a3921f19189575420cc7f8f946345b5";
  sha256 = "03q8rxpzy4l9nd4wv0107s0ck3dhfzkfy1rabf8srabkwl0c3vsc";
  fetchSubmodules = true;
}
