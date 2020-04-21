{ config, pkgs, ... }:

let
  pkgsWithOverlay = import <nixpkgs-unstable> {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/403c14c23be188b58c0b1bc197b428041d8a0cea.tar.gz;
      }))
    ];
  };

  # The emacs packages that I use
  # I differ between
  # - stable (Packages that I use for some time - happy with it)
  # - unstable (Packages that I use for some time - but may drop)
  # - testing (Packages that I try out - the new stuff)
  emacsPkgs = epkgs:
    (with epkgs.melpaPackages ;

    ## windows-purpose (testing)
    [ window-purpose ] ++

    ## helm (stable)
    # emacs completion engine
    [ helm helm-ag ] ++

    ## deft (testing)
    # text search for a directory
    [ deft ] ++

    ## lsp mode (unstable)
    # Language Server Protocol mode
    # Used for rust
    [ company-lsp dap-mode helm-lsp lsp-mode lsp-treemacs lsp-ui ] ++

    ## emacs convenience (stable)
    # Mixed and general purpose
    [ ag company direnv evil google-this spacemacs-theme ] ++

    ## common lisp (testing)
    [ slime ] ++

    ## magit (stable)
    [ magit ] ++

    ## bunch of programming languages (unstable)
    [ go-mode haskell-mode nix-mode ] ++

    ## rust (unstable)
    [ racer rust-mode ] ++

    ## python (stable)
    # Python IDE for emacs
    [ elpy ]) ++

    ## org-mode
    # Org-Mode has several extensions
    # and can be seen as an application of its own.
    (with epkgs.melpaPackages ;
    # testing
      [ org-super-agenda org-bullets org-ql ] ++
    # unstable
      [ smex org-mime orgit ]
    ) ++

    # stable
    (with epkgs.orgPackages ;
      [ org-plus-contrib ]) ++

    (with epkgs.elpaPackages ;
      [ bbdb which-key ]);

#    ## EXWM related (unstable)
#    epkgs.exwm
#    epkgs.melpaPackages.desktop-environment
#    epkgs.melpaPackages.helm-exwm
#  ];

  emacsWithOverlay = pkgsWithOverlay.emacsWithPackagesFromUsePackage {
    config = builtins.readFile ./elisp/init.el;
    # Package is optional, defaults to pkgs.emacs
    package = pkgsWithOverlay.emacsGit;
    # Optionally provide extra packages not in the configuration file
    extraEmacsPackages = emacsPkgs;
  };

  myEmacs = pkgs.writeDashBin "my-emacs" ''
    exec ${emacsWithOverlay}/bin/emacs -q "$@"
  '';

  myEmacsWithDaemon = pkgs.writeDashBin "my-emacs-daemon" ''
    exec ${emacsWithOverlay}/bin/emacs -q --daemon
  '';

  myEmacsClient = pkgs.writeDashBin "meclient" ''
    exec ${emacsWithOverlay}/bin/emacsclient --create-frame "$@"
  '';
in {
  environment.systemPackages = [
    myEmacs myEmacsWithDaemon myEmacsClient emacsWithOverlay
  ];

## EXWM Config
#  services.xserver = {
#    enable = true;
#    xkbOptions = "caps:super";
#    exportConfiguration = true;
#
#    displayManager.slim.enable = true;
#    windowManager.default = "exwm";
#
#    # Set up the login session
#    windowManager.session = [{
#      name = "exwm";
#      start = "${emacsWithOverlay}/bin/emacs -q -l " + builtins.toString ./elisp/init.el;
#    }];
#  };
}
