;;; Configs --- my emacs configs::
;;; Commentary:

;;; Code:
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
					

(add-to-list 'load-path "/folder/containing/file")
(require 'python)

;; initial window
(setq initial-frame-alist
      '(
        (width . 140) ; character
        (height . 40) ; lines
	(top . 100)
        (left . 250)
	))

;;; Disable startup message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;;; My personal information
(setq user-full-name "Ronaldo Lobato"
      user-mail-address "vieira.lobato@gmail.com"
      calendar-latitude 33.2471
      calendar-longitude 95.9000
      calendar-location-name "Commerce, Texas")


;;; List of my packages:
(setq package-list '(async auto-package-update anaconda-mode company company-anaconda company-web company-math company-auctex company-c-headers company-lsp dap-mode flycheck flycheck-color-mode-line flycheck-pos-tip helm helm-company helm-core helm-css-scss helm-lsp helm-sage helm-org jupyter langtool lsp-julia lsp-mode lsp-ui lsp-treemacs lsp-ivy magit synosaurus jedi jedi-core writegood-mode python-mode projectile ob-ipython ob-sagemath sage-shell-mode julia-mode julia-shell git-auto-commit-mode define-word function-args inflections math-symbol-lists rainbow-delimiters monokai-theme rainbow-mode window-numbering))


;mod test
;;Marmalade and Melpa
;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;(package-refresh-contents)


(display-time-mode 1)
; add below
; activate all the packages (in particular autoloads)
(package-initialize)

;theme
(load-theme 'monokai t)

; async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

;fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;mod teste
; orgmode.org hosts org elpa archives.
;(require 'package)
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;; auto-package-update
(auto-package-update-maybe)
;(auto-package-update-now)
(setq auto-package-update-interval 7)
(setq auto-package-update-prompt-before-update t)
(setq auto-package-update-delete-old-versions t)

; Emacs-langtool
(require 'langtool)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")

; set the default dictionary ispell
(setq ispell-dictionary "english")    ;set the default dictionary

;;auto-save
(setq auto-save-visited-file-name t)
(setq auto-save-visited-interval 60)

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

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

; lsp-mode
(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)

					; helm
(require 'helm-config)

					;projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

; dap-mode
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

					;rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Latex
(require 'package)
(package-initialize)

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
