{pkgs, python3Packages, ...}:

python3Packages.buildPythonPackage rec {
  name = "cacpy-${version}";
  version = "0.6.5";

  src = pkgs.fetchFromGitHub {
    owner = "makefu";
    repo = "python-cloudatcost";
    rev = "2bb4f940d4762938c06da380cd14767eafb171c9";
    sha256 = "1zl73q5iap76wfwjzvc25yqdrlmy9vqd7g4k31g5ig2ljy6sgwgc";
  };

  propagatedBuildInputs = with python3Packages; [
    docopt
    requests2
    beautifulsoup4
  ];
}

