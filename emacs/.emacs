    (defvar first-time t
      "Flag signifying this is the first time that .emacs has been evaled")

;; Meta
(global-set-key "\M- " 'set-mark-command)
(global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-h" 'help-command)

;; Mouse
(global-set-key [mouse-3] 'imenu)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Load packages
(require 'desktop)
(require 'tar-mode)

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
          "Intelligent Emacs interface to diff")

(if first-time
  (setq auto-mode-alist
        (append '(("\\.cpp$" . c++-mode)
                  ("\\.hpp$" . c++-mode)
                  ("\\.lsp$" . lisp-mode)
                  ("\\.scm$" . scheme-mode)
                  ("\\.cuf$" , f90-mode)
                  ("\\.pl$" . perl-mode)
                  ) auto-mode-alist)))

;; Auto font lock mode
(defvar font-lock-auto-mode-list
  (list 'c-mode 'c++-mode 'c++-c-mode 'emacs-lisp-mode 'lisp-mode 'perl-mode 'scheme-mode)
  "List of modes to always start in font-lock-mode")

(defvar font-lock-mode-keyword-alist
  '((c++-c-mode . c-font-lock-keywords)
    (perl-mode . perl-font-lock-keywords))
  "Associations between modes and keywords")

(defun font-lock-auto-mode-select ()
  "Automatically select font-lock-mode if the current major mode is in font-lock-auto-mode-list"
  (if (memq major-mode font-lock-auto-mode-list)
    (progn
      (font-lock-mode t))
    )
  )

;; New dabbrev stuff
;(require 'new-dabbrev)
(setq dabbrev-always-check-other-buffers t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (set (make-local-variable 'dabbrev-case-fold-search) nil)
             (set (make-local-variable 'dabbrev-case-replace) nil)))
(add-hook 'c-mode-hook
          '(lambda ()
             (set (make-local-variable 'dabbrev-case-fold-search) nil)
             `<             (set (make-local-variable 'dabbrev-case-replace) nil)))
(add-hook 'text-mode-hook
          '(lambda ()
             (set (make-local-variable 'dabbrev-case-fold-search) t)
             (set (make-local-variable 'dabbrev-case-replace) t)))

;; C++ and C mode...
(defun my-c++-mode-hook ()
  (setq tab-width 4)
  (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c++-mode-map "\C-ce" 'c-comment-edit)
  (setq c++-auto-hungry-initial-state 'none)
  (setq c++-delete-function 'backward-delete-char)
  (setq c++-tab-always-indent t)
  (setq c-indent-level 4)
  (setq c-continued-statement-offset 4)
  (setq c++-empty-arglist-indent 4))

(defun my-c-mode-hook ()
  (setq tab-width 4)
  (define-key c-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)
  (setq c-auto-hungry-initial-state 'none)
  (setq c-delete-function 'backward-delete-char)
  (setq c-tab-always-indent t)
  ;; BSD-ish indentation style
  (setq c-indent-level 4)
  (setq c-continued-statement-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 0)
  (setq c-label-offset -4))

;; Perl mode
(defun my-perl-mode-hook ()
  (setq tab-width 4)
  (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (setq perl-indent-level 4)
  (setq perl-continued-statement-offset 4))

;; Scheme mode...
(defun my-scheme-mode-hook ()
  (define-key scheme-mode-map "\C-m" 'reindent-then-newline-and-indent))

;; Emacs-Lisp mode...
(defun my-lisp-mode-hook ()
  (define-key lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key lisp-mode-map "\C-i" 'lisp-indent-line)
  (define-key lisp-mode-map "\C-j" 'eval-print-last-sexp))

;; Add all of the hooks...
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)


;;(add-to-list 'load-path "/home/john/.emacs.d/elpa/neotree")
;(require 'neotree)

;(require 'sublimity)
;(require 'sublimity-scroll)
;(require 'sublimity-map)
;(sublimity-global-mode)

;(require 'minimap)


;; Complement to next-error

(defun previous-error (n)
  "Visit previous compilation error message and corresponding source code."
  (interactive "p")
  (next-error (- n)))

;; Elisp archive searching
(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)

;; Font lock mode
(defun my-make-face (face color &optional bold)
  "Create a face from a color and optionally make it bold"
  (make-face face)
  (copy-face 'default face)
  (set-face-foreground face color)
  (if bold (make-face-bold face))
  )

;; TTY type terminal
(if (and (not window-system)
         (not (equal system-type 'ms-dos)))
  (progn
    (if first-time
      (progn
        (keyboard-translate ?\C-h ?\C-?)
        (keyboard-translate ?\C-? ?\C-h)))))


;; Restore the "desktop" - do this as late as possible
(if first-time
  (progn
    (desktop-load-default)
    (desktop-read)))

;; Indicate that this file has been read at least once
(setq first-time nil)

;; No need to debug anything now

(setq debug-on-error nil)

;; All done
(message "All done, %s%s" (user-login-name) ".")

;; (speedbar 1)

(setq c-recognize-knr-p nil)
(setq tab-width 4 indent-tabs-mode  nil)

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)
(setq warning-suppress-types nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(c-add-style "myCStyle"
             '("bsd"  ; this must be defined elsewhere - it is in cc-modes.el
               (c-basic-offset . 4)
               (c-echo-syntactic-information-p . t)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . (
                                   (c                     . c-lineup-C-comments)
                                   (statement-case-open   . 0)
                                   (case-label            . +)
                                   (substatement-open     . 0)
                                   ))
               ))

(global-set-key "\C-ca" 'list-matching-lines)
(global-set-key (kbd "C--") 'other-window)
(global-set-key [f12] 'compile)

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;;(load-file "~/Downloads/cedet-1.0.1/common/cedet.el")

(add-to-list 'load-path "/home/john/emacs/lisp/")

;;(require 'matlab-load)

;(load-file "/home/john/emacs/lisp/mathematica.el")
;(setq mathematica-command-line "/usr/local/bin/math")



(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)
; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
  This function is a custom function for tabbar-mode's tabbar-buffer-groups.
  This function group all buffers into 3 groups:
  Those Dired, those user buffer, and those emacs buffer.
  Emacs buffer are those starting with “*”."
  (list
    (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs Buffer"
       )
      ((eq major-mode 'dired-mode)
       "Dired"
       )
      (t
        "User Buffer"
        )
      ))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
        [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
        ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
 '(custom-safe-themes
        (quote
         ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(haskell-interactive-mode-hide-multi-line-errors nil)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(minimap-window-location (quote right))
 '(package-selected-packages
        (quote
         (ivy-yasnippet yasnippet-snippets swiper yasnippet counsel yaml-tomato salt-mode neotree elein whole-line-or-region company-shell company-anaconda helm-ls-git helm-anything helm-cider all-ext ace-isearch ace-jump-helm-line ac-helm helm lispy pretty-mode prettify-greek pretty-symbols latex-pretty-symbols hl-sexp cider-profile cider-decompile ac-cider paredit hlinum git-messenger erlang ensime clojure-mode)))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
        (quote
         ((20 . "#c82829")
          (40 . "#f5871f")
          (60 . "#eab700")
          (80 . "#718c00")
          (100 . "#3e999f")
          (120 . "#4271ae")
          (140 . "#8959a8")
          (160 . "#c82829")
          (180 . "#f5871f")
          (200 . "#eab700")
          (220 . "#718c00")
          (240 . "#3e999f")
          (260 . "#4271ae")
          (280 . "#8959a8")
          (300 . "#c82829")
          (320 . "#f5871f")
          (340 . "#eab700")
          (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "hack" :slant normal :weight normal :height 100 :width normal)))))
  

;;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")


;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(defmacro add-abbrevs (table &rest abr-list)                                    
  "Helper functions to add abbreviations to table"                              
  (let (bindings)                                                               
    (dolist (keycmd abr-list)                                                   
      (push `(define-abbrev ,table ,(car keycmd)                                
                            ,(car (cdr keycmd)) nil 0 t)                                     
            bindings))                                                          
    (push 'progn bindings)))                                                    


;;;Declare a list of abbreviations                                                                                

(defun my-f90-abbrevs ()
  "Add some abbreviations to make declaring allocatable arrays   easier"                                                        
  (add-abbrevs f90-mode-abbrev-table                                            
               ("`2d" "real, dimension(:,:), allocatable :: ")                  
               ("`3d" "real, dimension(:,:,:), allocatable :: ")))              


;;; We want f90-mode as default for editing fortran files                                                                                
(setq auto-mode-alist                                                           
      (append '(("\\.f90$" . f90-mode)
                ("\\.F90$" . f90-mode))                                         
              auto-mode-alist))        

;;; Customize f90-mode to allow for style guide recommendations
(add-hook 'f90-mode-hook                                                        
          (lambda ()                                                            
            (abbrev-mode 1)                                                     
            (my-f90-abbrevs)                                                    
            (setq f90-auto-keyword-case 'downcase-word                          
                  f90-do-indent 4                                               
                  f90-if-indent 4                                               
                  f90-type-indent 4                                             
                  f90-program-indent 4                                          
                  f90-continuation-indent 6                                     
                  f90-comment-region "!!!"                                      
                  f90-directive-comment "!!$"                                   
                  f90-indented-comment "!"                                      
                  f90-break-before-delimiters t                                 
                  f90-beginning-ampersand nil))) 
(put 'upcase-region 'disabled nil)

(defun turn-spell-checking-on ()
  "Turn flyspell-mode on."
  (flyspell-mode 1)
  )

(add-hook 'text-mode-hook 'turn-spell-checking-on)

(global-linum-mode 1) 

(defun mr-insert-current-time ()
  (interactive)
  (insert (format-time-string "%F %T")))
(global-set-key (kbd "C-c d") 'mr-insert-current-time)

(defun mr-insert-current-time-block ()
  (interactive)
  (insert "n-------------------n")
  (insert (format-time-string "%F %T"))
  (insert "n-------------------n")
  )
(global-set-key (kbd "C-c C-d") 'mr-insert-current-time-block)

(global-set-key (kbd "C-c o") 'occur)

(local-set-key (kbd "C-c d") 'delete-forward-char)


(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(require 'table)

(add-hook 'text-mode-hook 'table-recognize)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )


(autoload 'idomenu "idomenu" nil t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;(load "escreen")
;(escreen-install)

(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)

(global-set-key [s-left] 'windmove-left) 
(global-set-key [s-right] 'windmove-right) 
(global-set-key [s-up] 'windmove-up) 
(global-set-key [s-down] 'windmove-down)


;; make ido display choices vertically
(setq ido-separator "\n")

;; display any item that contains the chars you typed
(setq ido-enable-flex-matching t)

(global-set-key [M-down] 'enlarge-window)
(global-set-key [M-up] 'shrink-window)
(global-set-key [M-left] 'enlarge-window-horizontally)
(global-set-key [M-right] 'shrink-window-horizontally)

(require 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(setq tags-file-name "TAGS")

;(require 'expand-region)
;(global-set-key (kbd "C-=") 'er/expand-region)

;(require 'minimap)

(package-initialize)
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;;(color-theme-feng-shui)
;;(color-theme-sanityinc-tomorrow-eighties)
;; (color-theme-blue-sea)
;; (color-theme-euphoria)
(color-theme-hober)

(require 'hlinum)
(hlinum-activate)
(require 'git)

(autoload 'magit-status "magit" nil t)

(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(setq wolfram-program "/usr/local/Wolfram/Mathematica/10.0/SystemFiles/Kernel/Binaries/Linux-x86-64/MathKernel")
                                                                                                                 
                                                                                                                 

(add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))




(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'erlang-start)



(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)


(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode 1)
             ;; Actual keyboard bindings follow:

             (define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
             (define-key clojure-mode-map (kbd "s-i") 'cider-eval-last-sexp)
             (define-key clojure-mode-map (kbd "C-o y") 'cider-eval-last-sexp-and-append)))



;; Append result of evaluating previous expression (Clojure):
(defun cider-eval-last-sexp-and-append ()
  "Evaluate the expression preceding point and append result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-eval-and-get-value last-sexp)
    (with-current-buffer (current-buffer)
                         (insert ";;=>"))
    (cider-interactive-eval-print last-sexp)))




(add-to-list 'exec-path "/home/john/bin")

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq current-t43m3 nil)

(defun enab-theme (theme) 
  (if current-t43m3 (disable-theme current-t43m3))
  (setq current-t43m3 theme) 
  (load-theme theme t)) 

(defun disab-current-theme () 
  (if current-t43m3 (disable-theme current-t43m3))
  (setq current-t43m3 nil))

(global-set-key (kbd "C-c ltwo") '(lambda () (interactive) (enab-theme 'wombat)))

(global-set-key (kbd "C-c ltze") '(lambda () (interactive) (enab-theme 'zenburn)))

(global-set-key (kbd "C-c ltsd") '(lambda () (interactive) (enab-theme 'solarized-dark)))

(global-set-key (kbd "C-c ltsl") '(lambda () (interactive) (enab-theme 'solarized-light)))

(global-set-key (kbd "C-c ltne") '(lambda () (interactive) (enab-theme 'tomorrow-night-eighties)))

(global-set-key (kbd "C-c ltni") '(lambda () (interactive) (enab-theme 'tomorrow-night)))

(global-set-key (kbd "C-c ltnb") '(lambda () (interactive) (enab-theme 'tomorrow-night-bright)))

(global-set-key (kbd "C-c ltto") '(lambda () (interactive) (enab-theme 'tomorrow)))

(global-set-key (kbd "C-c ltta") '(lambda () (interactive) (enab-theme 'tango)))

(global-set-key (kbd "C-c ltdb") '(lambda () (interactive) (enab-theme 'deeper-blue)))

(global-set-key (kbd "C-c ltdi") '(lambda () (interactive) (enab-theme 'dichromacy)))


(defun l0ad-theme (name) 
  (interactive
    (list
      (intern (completing-read "Load custom theme: "
                               (mapcar 'symbol-name (custom-available-themes))))))
  (enab-theme name))

(setq d3fault-theme (getenv "EMACS_DEFAULT_THEME"))
(when d3fault-theme
  (enab-theme (intern d3fault-theme)))

(global-set-key (kbd "C-c v") (kbd "C-u - 1 6 C-x ^"))
(autoload 'cuf-mode "cuf-mode" t)
(autoload 'f90-mode "f90-mode" t)
(add-to-list 'auto-mode-alist '("\\.cuf\\'" . cuf-mode))
(add-to-list 'auto-mode-alist '("\\.f90\\'" . f90-mode))

(show-paren-mode 1)
(setq show-paren-style 'expression)

; display “lambda” as “λ” 
(global-prettify-symbols-mode 1)

(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist '( ("lambda" . 955) ; λ
                                 ("->" . 8594) ; →
;                                 ("=>" . 8658) ; ⇒
;                                 ("map" . 8614) ; ↦
                                 )))
(add-hook 'clojure-mode-hook 'my-add-pretty-lambda)
(add-hook 'haskell-mode-hook 'my-add-pretty-lambda)
(add-hook 'shen-mode-hook 'my-add-pretty-lambda)
(add-hook 'tex-mode-hook 'my-add-pretty-lambda)

(global-pretty-mode 1)

(font-lock-add-keywords
  'clojure-mode '(("(\\(fn\\)[\[[:space:]]" (0 (replacement-region "λ")))
                  ("\\(#\\)(" (0 (replacement-region "λ")))
                  ("(\\(partial\\)[[:space:]]" (0 (replacement-region "Ƥ")))
                  ("(\\(comp\\)[[:space:]]" (0 (replacement-region "ο")))))


(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

    
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


(global-set-key "\C-x\C-b" 'buffer-menu)

(require 'helm-config)

    
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

    
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))



  (add-hook 'yaml-mode-hook
            (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'color-theme-sanityinc-tomorrow)

;M-x color-theme-sanityinc-tomorrow-day
;M-x color-theme-sanityinc-tomorrow-night
;; M-x color-theme-sanityinc-tomorrow-blue
;; M-x color-theme-sanityinc-tomorrow-bright
                                        ;M-x color-theme-sanityinc-tomorrow-eighties
;; M-x color-theme-blue-sea
(windmove-default-keybindings)

(setq x-select-enable-clipboard t)

(add-to-list 'load-path
             "~/.emacs.d/elpa/yasnippet-20180621.50")
(require 'yasnippet)
(yas-global-mode 1)


(add-to-list 'load-path
             "~/.emacs.d/elpa/ivy-yasnippet-20180831.1615")

(ivy-mode 1)
(global-set-key "\C-s" 'swiper)


(setq yas-snippet-dirs '("~/emacs.d/snippets"
                         "~/.emacs.d/elpa/yasnippet-snippets-20180909.1015/snippets"))

;;Ivy, Counsel, Swiper Setup
;;
;;
;;
(ivy-mode 1) ;; Turn on ivy by default
(setq ivy-use-virtual-buffers t)  ;; no idea, but recommended by project maintainer
(setq enable-recursive-minibuffers t) ;; no idea, but recommended by project maintainer
(setq ivy-count-format "(%d/%d) ")  ;; changes the format of the number of results
(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; gives C-x C-f counsel features
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol) 
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)  
(global-set-key (kbd "C-c C-r") 'ivy-resume) 
 
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag) ;; add counsel/ivy features to ag package
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
 
;;set action options during execution of counsel-find-file
;; replace "frame" with window to open in new window
(ivy-set-actions
 'counsel-find-file
 '(("j" find-file-other-frame)))

;; set actions when running C-x b

;; replace "frame" with window to open in new window

(ivy-set-actions

 'ivy-switch-buffer

 '(("j" switch-to-buffer-other-frame "other
 "other frame")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("d" delete-file "delete")
   ("r" counsel-find-file-as-root "open as root")))
 frame")
   ("k" kill-buffer "kill")
   ("r" ivy--rename-buffer-action "rename")))
 
;;
;;
;; End Ivy, Swiper, Counsel

;; version of ivy-yank-word to yank from start of word
(defun bjm/ivy-yank-whole-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (let (amend)
    (with-ivy-window
      ;;move to last word boundary
      (re-search-backward "\\b")
      (let ((pt (point))
            (le (line-end-position)))
        (forward-word 1)
        (if (> (point) le)
            (goto-char pt)
          (setq amend (buffer-substring-no-properties pt (point))))))
    (when amend
      (insert (replace-regexp-in-string "  +" " " amend)))))

;; bind it to M-j
(define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word)
