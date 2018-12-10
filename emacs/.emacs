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
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:family "Andale Mono" :foundry "monotype" :slant normal :weight normal :height 85 :width normal)))))
 '(default ((t (:family "Monaco" :slant normal :weight normal :height 72 :width normal)))))

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

(add-to-list 'load-path "/home/john/.emacs.d/elpa/neotree")
(require 'neotree)

                                        ;(require 'sublimity)
                                        ;(require 'sublimity-scroll)
                                        ;(require 'sublimity-map)
                                        ;(sublimity-global-mode)

                                        ;(require 'minimap)


(autoload 'idomenu "idomenu" nil t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(load-theme 'misterioso)

(load "escreen")
(escreen-install)


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
