(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("e2a06cbad59e7a19c40c689b4bcb84344fe6a375e7c237ad991ef1aa587e9caa" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (lsp-treemacs lsp-ui company-lsp flycheck company dash dash-functional f git-commit ivy js2-mode julia-mode markdown-mode polymode pos-tip transient websocket with-editor ob-sagemath python-mode synosaurus magit auctex auto-complete company-web web-completion-data company-c-headers git-auto-commit-mode helm-company yasnippet anaconda-mode async company-math define-word helm helm-core jedi-core jupyter math-symbol-lists zmq virtualenv langtool magit-popup pythonic real-auto-save apiwrap auto-complete-sage auto-package-update company-anaconda company-auctex concurrent ctable deferred epc fortpy function-args ghub graphql helm-css-scss helm-sage inflections jedi julia-shell ob-ipython popup python-environment s sage-shell-mode treepy writegood-mode ghub+ diminish cl-generic direx folding let-alist request request-deferred simple-httpd skewer-mode swiper python3-info python-x log4e jedi-direx gntp ein ac-anaconda)))
 '(vc-annotate-background "#ffffff")
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
 '(vc-annotate-very-old-color "#585858"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "/folder/containing/file")
(require 'python)

;; initial window
(setq initial-frame-alist
      '(
        (width . 90) ; character
        (height . 30) ; lines
        ))


; list the packages
(setq package-list '(auto-package-update anaconda-mode company company-anaconda company-web company-math company-auctex company-c-headers company-lsp flycheck helm-lsp jupyter langtool lsp-mode lsp-ui lsp-treemacs lsp-ivy magit real-auto-save synosaurus jedi jedi-core writegood-mode python-mode ob-ipython ob-sagemath sage-shell-mode julia-mode julia-shell helm helm-core helm-sage helm-company helm-css-scss git-auto-commit-mode define-word function-args inflections math-symbol-lists))

;;Marmalade and Melpa
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; orgmode.org hosts org elpa archives.
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

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

; lsp-mode
(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'fortran-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

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
