(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(beacon-color "#cc6666")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#11948b")
 '(cua-normal-cursor-color "#596e76")
 '(cua-overwrite-cursor-color "#a67c00")
 '(cua-read-only-cursor-color "#778c00")
 '(custom-enabled-themes (quote (monokai)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(fci-rule-color "#f4eedb")
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(font-use-system-font nil)
 '(frame-background-mode (quote dark))
 '(highlight-changes-colors (quote ("#c42475" "#5e65b6")))
 '(highlight-symbol-colors
   (quote
    ("#ec90da49b1e9" "#ccb4e1bdd0ac" "#fb9eca14b38f" "#d89bd3eadcf9" "#de29dee7b293" "#f675cca1ae79" "#d05fdab7e079")))
 '(highlight-symbol-foreground-color "#5d737a")
 '(highlight-tail-colors
   (quote
    (("#f4eedb" . 0)
     ("#a8b84b" . 20)
     ("#66c1b3" . 30)
     ("#6fa5e7" . 50)
     ("#d6a549" . 60)
     ("#ed6e3e" . 70)
     ("#f46495" . 85)
     ("#f4eedb" . 100))))
 '(hl-bg-colors
   (quote
    ("#d6a549" "#ed6e3e" "#ff6243" "#f46495" "#837bdf" "#6fa5e7" "#66c1b3" "#a8b84b")))
 '(hl-fg-colors
   (quote
    ("#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9")))
 '(hl-paren-colors (quote ("#11948b" "#a67c00" "#007ec4" "#5e65b6" "#778c00")))
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-border "#5d737a")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#cc1f24" "#bb3e06" "#a67c00" "#4f6600" "#a8b84b" "#005797" "#11948b" "#c42475" "#5e65b6")))
 '(package-selected-packages
   (quote
    (org-caldav window-numbering flycheck-pos-tip flycheck-color-mode-line helm-org zenburn-theme rainbow-mode monokai-theme rainbow-delimiters projectile lsp-ivy helm-lsp lsp-ui company-lsp flycheck dash dash-functional f js2-mode ob-sagemath synosaurus auctex auto-complete company-web web-completion-data company-c-headers helm-company anaconda-mode company-math define-word jedi-core jupyter math-symbol-lists zmq virtualenv pythonic real-auto-save apiwrap auto-complete-sage auto-package-update company-anaconda company-auctex concurrent ctable deferred epc fortpy function-args graphql helm-sage inflections jedi julia-shell ob-ipython popup python-environment s sage-shell-mode treepy writegood-mode diminish cl-generic direx folding let-alist request-deferred simple-httpd python3-info python-x log4e jedi-direx gntp ac-anaconda)))
 '(pos-tip-background-color "#f4eedb")
 '(pos-tip-foreground-color "#5d737a")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#778c00" "#f4eedb" 0.2))
 '(term-default-bg-color "#fffce9")
 '(term-default-fg-color "#596e76")
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ba8baf")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858")
 '(weechat-color-list
   (quote
    (unspecified "#fffce9" "#f4eedb" "#990001" "#cc1f24" "#4f6600" "#778c00" "#785700" "#a67c00" "#005797" "#007ec4" "#93004d" "#c42475" "#006d68" "#11948b" "#596e76" "#88999b")))
 '(xterm-color-names
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#002b37"])
 '(xterm-color-names-bright
   ["#fffce9" "#bb3e06" "#98a6a6" "#88999b" "#596e76" "#5e65b6" "#5d737a" "#00212b"]))

;no mod above					;
;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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


					;
					;rainbow conf above

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


; list the packages
(setq package-list '(async auto-package-update anaconda-mode company company-anaconda company-web company-math company-auctex company-c-headers company-lsp dap-mode flycheck flycheck-color-mode-line flycheck-pos-tip helm helm-company helm-core helm-css-scss helm-lsp helm-sage helm-org jupyter langtool lsp-julia lsp-mode lsp-ui lsp-treemacs lsp-ivy magit real-auto-save synosaurus jedi jedi-core writegood-mode python-mode projectile ob-ipython ob-sagemath sage-shell-mode julia-mode julia-shell git-auto-commit-mode define-word function-args inflections math-symbol-lists rainbow-delimiters monokai-theme rainbow-mode window-numbering))


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

; auto-package-update
(auto-package-update-maybe)
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
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'LaTeX-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 60)

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
