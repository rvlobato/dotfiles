;;; Configs --- my emacs configs::
;;; Commentary:
;;; Code:
;; Set customization data in a specific file, without littering init files.
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

;; Make all commands of the package module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 )
      )

;; Activate all the packages (in particular autoloads)
(package-initialize)
;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Pin third-party to MELPA Stable
(setq use-package-always-pin "melpa stable")

;;; Install my packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Auto package update
(use-package auto-package-update
  :defer 10
  :config
  ;; The periodicity (in days)
  (setq auto-package-update-interval 30)
  ;; ask before automatic update
  (setq auto-package-update-prompt-before-update t)
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;;; Read my configs
(org-babel-load-file "~/.emacs.d/configuration.org")
;;; init.el ends here
