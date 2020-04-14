{ config, pkgs, ... }:

let
  pkgsWithOverlay = import <nixpkgs-unstable> {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/403c14c23be188b58c0b1bc197b428041d8a0cea.tar.gz;
      }))
    ];
  };

  #emacsWithCustomPackages
  emacsPkgs= epkgs: [
    # testing lsp mode
    epkgs.melpaPackages.lsp-ui
    epkgs.melpaPackages.company-lsp
    epkgs.melpaPackages.lsp-treemacs
    epkgs.melpaPackages.helm-lsp
    epkgs.melpaPackages.dap-mode
    epkgs.melpaPackages.lsp-mode

    # testing
    epkgs.melpaPackages.web-mode
    epkgs.melpaPackages.js2-mode
    epkgs.melpaPackages.xref-js2

    epkgs.melpaPackages.academic-phrases

    epkgs.melpaPackages.gitlab
    epkgs.melpaPackages.weechat

# helm
    epkgs.melpaPackages.helm
    epkgs.melpaPackages.helm-fuzzier
    epkgs.melpaPackages.helm-ag


# emacs convenience
    epkgs.melpaPackages.ag
    epkgs.melpaPackages.company
    epkgs.melpaPackages.direnv
    epkgs.melpaPackages.evil
    epkgs.melpaPackages.google-this
    epkgs.melpaPackages.monokai-alt-theme
    epkgs.melpaPackages.spacemacs-theme
    epkgs.melpaPackages.zenburn-theme

# development
    epkgs.melpaPackages.magit
    epkgs.melpaPackages.nix-mode
    epkgs.melpaPackages.go-mode
    epkgs.melpaPackages.haskell-mode
# rust
    epkgs.melpaPackages.rust-mode
#    epkgs.melpaPackages.flycheck-rust
    epkgs.melpaPackages.racer

# python
    epkgs.melpaPackages.elpy

    # org-mode
    epkgs.melpaPackages.org-super-agenda
    epkgs.melpaPackages.org-bullets
    epkgs.melpaPackages.org-ql

    epkgs.elpaPackages.bbdb
    epkgs.orgPackages.org-plus-contrib
    epkgs.melpaPackages.smex
    epkgs.melpaPackages.org-mime
    epkgs.melpaPackages.orgit

    epkgs.elpaPackages.which-key

    epkgs.exwm
    epkgs.melpaPackages.desktop-environment
    epkgs.melpaPackages.helm-exwm
  ];

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
