;;; Configs --- my emacs configs::
;;; Commentary:
;;; Code:
;; Set customization data in a specific file, without littering init files.
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

;; Make all commands of the package module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "http://melpa.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)
;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;; Install my packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Auto package update
(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;;; Read my configs
(org-babel-load-file "~/.emacs.d/configuration.org")
;;; init.el ends here
