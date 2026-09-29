;; -*- lexical-binding: t; -*-
;;; init.el --- Minimal entry point -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(require 'org)
(require 'use-package)

(defun my/add-lexical-binding-to-tangled-el ()
  "Ensure tangled .el files begin with a lexical-binding declaration."
  (when (and buffer-file-name (string-match-p "\\.el\\'" buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at-p ";;.*lexical-binding")
        (insert ";; -*- lexical-binding: t; -*-\n\n"))
      (save-buffer))))

(add-hook 'org-babel-post-tangle-hook #'my/add-lexical-binding-to-tangled-el)

(org-babel-load-file
 (expand-file-name "configuration.org" user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)
