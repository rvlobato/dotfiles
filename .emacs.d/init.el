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

;;; List of my packages:
(setq package-selected-packages '(async auto-package-update anaconda-mode company company-anaconda company-auctex company-c-headers company-math company-web dap-mode flycheck flycheck-color-mode-line flycheck-pos-tip helm helm-company helm-core helm-css-scss helm-lsp helm-sage helm-org jupyter langtool lsp-julia lsp-mode lsp-ui lsp-treemacs lsp-ivy magit synosaurus jedi jedi-core writegood-mode python-mode projectile ob-ipython ob-sagemath sage-shell-mode julia-mode julia-shell define-word function-args inflections math-symbol-lists rainbow-delimiters rainbow-mode window-numbering zotxt))

;;; Install my packages
(dolist (package package-selected-packages)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))
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

;; Initial window size and position on screen
(setq initial-frame-alist
      '((width . 145) ; character
        (height . 45) ; lines
	(top . 200)   ; position on the screen
	(left . 350)))

;;; Disable startup message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;;; Goes to the last place where it was when you previously visited the same file
(setq-default save-place  t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

;; Mods in the modeline
(display-time-mode 1) ;; Display of time
(tool-bar-mode   -1)  ;; No large icons
(scroll-bar-mode -1)  ;; No visual indicator
(menu-bar-mode   -1)  ;; The Mac OS top pane has menu options

;; Highlight matching ‘parenthesis’
(setq show-paren-delay  0)
(setq show-paren-style 'mixed)
(show-paren-mode)

;; Change buffer names for files with the same name. Note that ‘uniquify’ is builtin.
(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

;; Make it easier to discover key shortcuts
(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.5))

;; Do not show some common modes in the modeline, to save space
(use-package diminish
  :defer 5
  :config
  (diminish 'org-indent-mode))

;; Magit
;(use-package magit
;  :config
;  (global-set-key (kbd "C-x g") 'magit-status))

;; Switch windows with shift-arrows instead of "C-x o" all the time
(windmove-default-keybindings)

;; Theme
(use-package flatland-theme
  :config
  (custom-theme-set-faces 'flatland
   '(show-paren-match ((t (:background "dark gray" :foreground "black" :weight bold))))
   '(show-paren-mismatch ((t (:background "firebrick" :foreground "orange" :weight bold))))))

;;; My personal information
(setq user-full-name "Ronaldo V. Lobato"
      user-mail-address "vieira.lobato@gmail.com"
      calendar-latitude 33.2471
      calendar-longitude 95.9000
      calendar-location-name "Commerce, Texas")
				
;;; Desired colored output shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

;;; Emacs-langtool
(require 'langtool)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")

;;; Flyspell mode
(use-package flyspell
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         ((org-mode text-mode) . flyspell-mode)))

; Set the default dictionary ispell
(setq ispell-dictionary "english")

(use-package synosaurus
  :diminish synosaurus-mode
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'popup) ;; 'ido is default.
           (global-set-key (kbd "M-#") 'synosaurus-choose-and-replace))

;;auto-save
(setq auto-save-visited-file-name t)
(setq auto-save-visited-interval 300)

; Python autocompletation
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

; anaconda-mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

; company-mode
(add-hook 'after-init-hook 'global-company-mode)

; company-anaconda
(eval-after-load "company"
'(add-to-list 'company-backends 'company-anaconda))

; company-web
(require 'company)                                   ; load company mode
(require 'company-web-html)                          ; load company mode html backend

; company-math
;; global activation of the unicode symbol completion
(add-to-list 'company-backends 'company-math-symbols-unicode)

; company-auctex
(require 'company-auctex)
(company-auctex-init)

; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

; lsp-mode
(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)

;;; Helm
(use-package helm
 :diminish
 :init (helm-mode t)
 :bind (("M-x"     . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b"   . helm-mini)     ;; See buffers & recent files; more useful.
        ("C-x r b" . helm-filtered-bookmarks)
        ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
        ("C-c i"   . helm-imenu)
        ("C-h a"   . helm-apropos)
        ;; Look at what was cut recently & paste it in.
        ("M-y" . helm-show-kill-ring)

        :map helm-map
        ;; We can list ‘actions’ on the currently selected item by C-z.
        ("C-z" . helm-select-action)
        ;; Let's keep tab-completetion anyhow.
        ("TAB"   . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action)))

;; Ripgrep
(use-package rg
  :config
  (global-set-key (kbd "M-s g") 'rg)
  (global-set-key (kbd "M-s d") 'rg-dwim))
(use-package helm-rg)

;; Rustic, LSP
(use-package rustic)
(use-package lsp-ui)
(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;;; Projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

; dap-mode
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))


; Julia
(add-to-list 'load-path "path-to-julia-shell-mode")
(require 'julia-shell)

(add-to-list 'load-path "path-to-julia-mode")
(require 'julia-mode)

(defun customize-julia-mode ()
  "Customize julia-mode."
  (interactive)
  ;; my customizations go here
  )

(add-hook 'julia-mode-hook 'customize-julia-mode)

; c++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;ipython notebook
(setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
;; (setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)

;;ob-ipython
(require 'ob-ipython)

;;auto revert mode refresh
(global-auto-revert-mode 1)

(eval-after-load "sage-shell-mode"
  '(sage-shell:define-keys sage-shell-mode-map
     "C-c C-i"  'helm-sage-complete
     "C-c C-h"  'helm-sage-describe-object-at-point
     "M-r"      'helm-sage-command-history
     "C-c o"    'helm-sage-output-history))

(require 'sage-shell-mode)

;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

;; Turn on eldoc-mode in Sage terminal and in Sage source files
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

;; If you have Sage 7.4 or later, uncomment the following line.
(setq sage-shell:use-prompt-toolkit t)

;;Inline display of LaTeX outputs and plots (a port of sage-view)
;; If you want to enable inline display of LaTeX outputs only,
;; uncomment the following line.
(setq sage-shell-view-default-commands 'output)

;; If you want to enable inline display of plots only,
;; uncomment the following line.
(setq sage-shell-view-default-commands 'plot)

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)

;;ob-sagemath
;; Ob-sagemath supports only evaluating with a session.
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))

;; C-c c for asynchronous evaluating (only for SageMath code blocks).
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;;; Rainbow mode
(require 'rainbow-mode)
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)

;;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;; rainbow colors conf:
(custom-set-faces
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta3"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "tomato"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "gold"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium spring green"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "gainsboro")))))

;;; EOF
;;; init.el ends here
