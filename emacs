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
    (git-auto-commit-mode git-commit helm-company python-mode with-editor yasnippet anaconda-mode async company company-math dash define-word helm helm-core ivy jedi-core julia-mode jupyter math-symbol-lists websocket zmq virtualenv langtool magit magit-popup pythonic real-auto-save synosaurus apiwrap auctex auto-complete auto-complete-sage auto-package-update company-anaconda company-auctex concurrent ctable dash-functional deferred epc f fortpy function-args ghub graphql helm-css-scss helm-sage inflections jedi julia-shell ob-ipython ob-sagemath popup pos-tip python-environment s sage-shell-mode treepy writegood-mode ghub+ diminish cl-generic direx folding let-alist request request-deferred simple-httpd skewer-mode swiper python3-info python-x log4e jedi-direx gntp ein ac-anaconda)))
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
(setq package-list '(jupyter langtool magit real-auto-save synosaurus jedi jedi-core anaconda-mode writegood-mode python-mode ob-ipython ob-sagemath sage-shell-mode julia-mode julia-shell helm helm-core helm-sage helm-company helm-css-scss git-auto-commit-mode fortpy define-word company company-math company-anaconda company-auctex auto-package-update auto-complete auto-complete-sage function-args inflections math-symbol-lists))

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

; Fortran
; Standard fortpy.el setting
(add-hook 'f90-mode-hook 'fortpy-setup)
(setq fortpy-complete-on-percent t)
(setq fortpy-complete-on-bracket t)

; Latex
(require 'package)
(package-initialize)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

; company-mode
(add-hook 'after-init-hook 'global-company-mode)

; company-math
; global activation of the unicode symbol completion 
(eval-after-load "company"
'(add-to-list 'company-backends 'company-math-symbols-latex))

; company-anaconda
(eval-after-load "company"
'(add-to-list 'company-backends 'company-anaconda))

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

;;auto-complete Sage
(add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
(add-hook 'sage-shell-mode-hook 'ac-sage-setup)

;;(eval-after-load "auto-complete-sage"
;;  '(setq sage-shell:completion-function 'completion-at-point))

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
