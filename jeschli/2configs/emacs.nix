{ config, pkgs, ... }:

let
  emacsFile = ''
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t)
  (add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") t)
)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Evil Mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(require 'evil-org)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (smex ox-jira org-plus-contrib org-mime org-jira neotree molokai-theme let-alist helm-fuzzy-find go-guru go-autocomplete flymake-go exec-path-from-shell evil-org cl-lib-highlight bbdb atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq tab-width 2)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell
  '';
  dotEmacs = pkgs.writeText "dot-emacs" emacsFile;
  emacs = (pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    magit
  ]));
  myEmacs = pkgs.writeDashBin "my-emacs" ''
    exec ${emacs}/bin/emacs -q -l ${dotEmacs} "$@"
  '';
in {
  environment.systemPackages = [
    myEmacs
  ];
}
