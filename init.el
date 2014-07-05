;;; init.el --- This file is loaded when Emacs starts up.
;;
;; Copyright © 2013 Ron Mordechai
;;
;; Author: Ron Mordechai <ronmrdechai@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My Emacs configuration.  Geared towards C/C++, Python, ELisp, Perl,
;; JavaScript and assembly development.

;;; License: The MIT License

;; Copyright (c) 2013 Ron Mordechai

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Code:

;; -----------------------------------------------------------------------------
;; SANER DEFAULTS

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; disable line wrapping
(setq-default truncate-lines t)

;; misc. settings
(setq redisplay-dont-pause t
      inhibit-splash-screen t
      require-final-newline t
      next-line-add-newlines nil
      initial-scratch-message nil
      confirm-nonexistent-file-or-buffer nil
      default-directory "~/"
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      gc-cons-threshold 20000000
      large-file-warning-threshold 100000000)

;; set Emacs' source directory to allow easy browsing
(setq source-directory "~/Documents/code/downloads/emacs")

;; scroll line by line
(setq scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; back stuff up to .emacs.d/cache/backup
(setq make-backup-files t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/cache/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      delete-by-moving-to-trash t)

;; enable case insensitive file completion
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; keep the auto-save-list in .emacs.d/cache/auto-save-list
(setq auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/saves-")

;; turn yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; show column number as well as line number
(column-number-mode 1)

;; delete highlighted text
(delete-selection-mode t)

;; turn on font-lock mode (syntax highlighting)
(global-font-lock-mode t)

;; custom set stuff
(custom-set-variables
 '(recentf-auto-cleanup 300))

;; enable disabled functions
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; set Monokai as the theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'monokai t)

;; add /usr/local/bin to `exec-path' and PATH
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; TODO: fix issue with opening frames through emacsclient
(defun on-frame-open (frame)
  "A list of actions to perform before opening FRAME."
  (if (display-graphic-p frame)
      (progn
        (mwheel-install)
        (setq x-select-enable-clipboard t)
        (set-selection-coding-system 'compound-text-with-extensions)
        (tool-bar-mode 0)
        (scroll-bar-mode 0)
        (set-frame-font "Monaco-10" nil (list frame))
        (set-frame-size frame 80 50)
        (set-frame-parameter frame 'alpha 92))
    (menu-bar-mode 0)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

;; -----------------------------------------------------------------------------
;; MODELINE OPTIONS

;; make the modeline flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :background "#141414" :box nil)

(defun mode-line-fill (&optional reserve)
  "Return empty space leaving RESERVE space on the right."
  (let ((reserve (or reserve 20)))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,reserve))))))

(defun vc-branch-name (&optional max-len)
  "Get branch of current file, truncate to MAX-LEN if needed."
  (let ((name (vc-working-revision (buffer-file-name)))
        (max-len (or max-len 15)))
    (if (> (length name) max-len)
        (concat (substring name 0 (- max-len 3)) "...") name)))

;; Set the modeline to something nice
(setq-default
 mode-line-format
 (list mode-line-frame-identification
       mode-line-buffer-identification " "
       mode-line-position '(:eval mode-name) "   "
       '(:eval
         (propertize (if (and overwrite-mode (buffer-modified-p))
                         "*modif-ovwrt*"
                       (if (buffer-modified-p) "*modified*"
                         (if overwrite-mode "*overwrite*" "")))
                     'face 'font-lock-builtin-face
                     'help-echo "Buffer has been modified"))
       (mode-line-fill 12)
       '(:eval (vc-branch-name 12)) ))

;; -----------------------------------------------------------------------------
;; PACKAGE.EL OPTIONS

(require 'package)
(defun add-to-package-archives (element)
  "Add ELEMENT to list `package-archives'."
  (add-to-list 'package-archives element))
(mapc 'add-to-package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  ;; check for new packages (package versions)
  (message "Refreshing package database...")
  (package-refresh-contents)
  (message "done."))

;; make sure packages are installed on startup
(defvar my-packages
  '(ac-slime   auto-complete  concurrent  ctable  dash
    epc epl expand-region impatient-mode jedi js2-mode
    multiple-cursors   pkg-info   popup   rainbow-mode
    smartparens smex tern tern-auto-complete)
  "A list of packages to ensure are installed at launch.")

(defun packages-installed-p ()
  "Check if all packages are installed."
  (require 'cl-lib)
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (packages-installed-p)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; -----------------------------------------------------------------------------
;; GENERAL OPTIONS

;; add .emacs.d/site-lisp to load-path
(add-to-list 'load-path "~/.emacs.d/lisp/external")

;; -----------------------------------------------------------------------------
;; RECTANGULAR MARK OPTIONS

(defvar rm-mark-active nil)
(autoload 'rm-set-mark                "rect-mark" nil t)
(autoload 'rm-kill-region             "rect-mark" nil t)
(autoload 'rm-kill-ring-save          "rect-mark" nil t)
(autoload 'rm-mouse-drag-region       "rect-mark" nil t)
(autoload 'rm-exchange-point-and-mark "rect-mark" nil t)
(global-set-key (kbd "<C-return>") 'rm-set-mark)
(global-set-key (kbd "C-w")
                (lambda(b e) (interactive "r")
                  (if rm-mark-active (rm-kill-region b e)
                    (kill-region b e))))
(global-set-key (kbd "M-w")
                (lambda(b e) (interactive "r")
                  (if rm-mark-active (rm-kill-ring-save b e)
                    (kill-ring-save b e))))
(global-set-key (kbd "C-x C-x")
                (lambda(&optional p) (interactive "p")
                  (if rm-mark-active
                      (rm-exchange-point-and-mark p)
                    (exchange-point-and-mark p))))
(global-set-key (kbd "C-x <down-mouse-1>") 'rm-mouse-drag-region)

;; -----------------------------------------------------------------------------
;; MULTIPLE CURSORS OPTIONS

(setq mc/list-file "~/.emacs.d/cache/mc-lists.el")
(autoload 'mc/edit-lines              "mc-edit-lines" nil t)
(autoload 'mc/mark-next-like-this     "mc-mark-more"  nil t)
(autoload 'mc/mark-previous-like-this "mc-mark-more"  nil t)
(autoload 'mc/mark-all-like-this      "mc-mark-more"  nil t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;; -----------------------------------------------------------------------------
;; WINDMOVE OPTIONS

(autoload 'windmove-left  "windmove" nil t)
(autoload 'windmove-right "windmove" nil t)
(autoload 'windmove-up    "windmove" nil t)
(autoload 'windmove-down  "windmove" nil t)
(global-set-key (kbd "<s-left>")  'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-up>")    'windmove-up)
(global-set-key (kbd "<s-down>")  'windmove-down)

;; use `framemove' if `windmove' fails
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; -----------------------------------------------------------------------------
;; MISC MODE OPTIONS

;; llvm-mode
(autoload 'llvm-mode "llvm-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode))

;; markdown-mode
(autoload 'gfm-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-hook 'gfm-mode-hook 'linum-on)

;; 6502-mode
(autoload '6502-mode "6502-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.s65$" . 6502-mode))
(add-hook '6502-mode-hook
		  (lambda ()
            (ad-disable-advice 'yank 'around 'yank-and-indent)
			(local-set-key (kbd "RET") 'newline-and-indent)
			(electric-indent-mode -1)))

;; asm86-mode
(autoload 'asm86-mode "asm86-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.s$" . asm86-mode))

;; linum-mode
(autoload 'linum-on   "linum" nil t)
(autoload 'linum-mode "linum" nil t)
(eval-after-load "linum" '(setq linum-format " %d"))

;; python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (electric-indent-mode -1)))

;; -----------------------------------------------------------------------------
;; MODE COMPILE OPTIONS

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c m c") 'mode-compile)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c m k") 'mode-compile-kill)

;; -----------------------------------------------------------------------------
;; SLIME OPTIONS

;; slime
(add-to-list 'load-path "~/.emacs.d/lisp/packages/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/ccl")
(setq slime-contribs '(slime-fancy))

(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-hook 'lisp-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (save-excursion (slime)))
            (slime-mode t)))

(setq slime-complete-symbol-function  'slime-fuzzy-complete-symbol
      slime-fuzzy-completion-in-place t
      slime-enable-evaluate-in-emacs  t
      slime-autodoc-use-multiline-p   t)
(make-directory "/tmp/slime-fasls/" t)
(setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))

(add-hook 'slime-mode-hook      'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; -----------------------------------------------------------------------------
;; TAGS FILES OPTIONS

;; libc:
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'tags-table-list "~/.emacs.d/data/LIBCTAGS")))
;; libc++:
(add-hook 'c++-mode-hook
          (lambda ()
            (add-to-list 'tags-table-list "~/.emacs.d/data/LIBCTAGS")
            (add-to-list 'tags-table-list "~/.emacs.d/data/LIBCXXTAGS")))
;; elisp:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'tags-table-list "~/.emacs.d/data/STDLISPTAGS")))

;; -----------------------------------------------------------------------------
;; FLYSPELL/CHECK OPTIONS

;; flyspell
(setq-default ispell-program-name "/usr/local/bin/aspell")
(eval-after-load "flyspell"
  '(progn
     (setq ispell-personal-dictionary "~/.emacs.d/data/dict-custom")
     ;; (fset 'ispell-get-word 'ispell-get-cC-word)
     (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))
(when (executable-find ispell-program-name)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load "flycheck"
  '(progn (setq flycheck-highlighting-mode 'symbols)
          ;; remove javascript checkers, js2-mode does the linting
          (setq-default flycheck-disabled-checkers
                        (remove-if-not
                         (lambda (s) (string-prefix-p
                                      "javascript"
                                      (symbol-name s)))
                         flycheck-checkers))))

;; -----------------------------------------------------------------------------
;; ESHELL OPTIONS

(defcustom eshell-directory-name
  (locate-user-emacs-file "cache/eshell/")
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)
(eval-after-load "esh-mode"
  (setq eshell-path-env (mapconcat (lambda (x) x) exec-path ":")))

;; -----------------------------------------------------------------------------
;; PERL OPTIONS

;; set cperl-mode as the default perl mode
(defalias 'perl-mode 'cperl-mode)
(eval-after-load "cperl-mode"
  '(progn
     (setq cperl-indent-level 4)
     (setq font-lock-maximum-decoration '((cperl-mode . 0) (t . t)))))

;; -----------------------------------------------------------------------------
;; JAVASCRIPT OPTIONS

;; enable js2-mode for javascript, and tern for completion
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(eval-after-load "tern"
  '(progn (require 'tern-auto-complete)
          (tern-ac-setup)
          (define-key js2-mode-map (kbd "C-`") 'tern-ac-complete)))
(eval-after-load "js2-mode"
  '(progn
     (setq js2-show-parse-errors nil
           js2-skip-preprocessor-directives t
           js2-cleanup-whitespace t)
     (define-key js2-mode-map (kbd "C-c h") 'js2-mode-toggle-element)))

;; -----------------------------------------------------------------------------
;; IDO OPTIONS

(eval-after-load "ido"
  '(progn
     (setq ido-case-fold t
	   ido-max-prospects 8
	   ido-confirm-unique-completion t
	   ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
     (add-to-list 'ido-ignore-files "\\.DS_Store")
     (mapc
      (lambda (regexp)
	(add-to-list 'ido-ignore-buffers regexp))
      '("^EMACS_TAGS.*$" "^STDLISPTAGS$" "^LIBC(XX)?TAGS$" "TAGS"))))
(autoload 'ido-mode "ido" nil t)
(ido-mode 'both)

;; -----------------------------------------------------------------------------
;; RECENTF OPTIONS

(eval-after-load "recentf"
  '(progn
     (setq recentf-save-file "~/.emacs.d/cache/recentf"
           recentf-max-saved-items 100
           recentf-max-menu-items 15)
     (mapc
      (lambda (regexp)
        (add-to-list 'recentf-exclude regexp))
      '("^EMACS_TAGS.*$" "^STDLISPTAGS$" "^LIBC(XX)?TAGS$" "TAGS"))))
(autoload 'recentf-mode "recentf" nil t)
(recentf-mode 1)

;; -----------------------------------------------------------------------------
;; IBUFFER OPTIONS

;; ibuffer configuration
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs" (or (name . "^\\*.*\\*$")))
         ("TAGS"  (or (name . "^EMACS_TAGS.*$")
                      (name . "^STDLISPTAGS$")
                      (name . "^LIBC(XX)?TAGS$")
                      (name . "TAGS"))))))
(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; -----------------------------------------------------------------------------
;; SMEX OPTIONS

(autoload 'smex "smex" nil t)
(autoload 'smex-major-mode-commands "smex" nil t)
(eval-after-load "smex"
  (progn
    (setq smex-save-file "~/.emacs.d/cache/smex-items")))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; -----------------------------------------------------------------------------
;; COMPLETION OPTIONS

;; activate autocomplete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
(ac-flyspell-workaround)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)

;; irony-mode for c
(if (file-executable-p ".emacs.d/lisp/packages/irony-mode/bin/irony-server")
    (add-hook 'c-mode-common-hook
              (lambda ()
                (add-to-list 
		 'load-path
		 "/Users/Ron/.emacs.d/lisp/packages/irony-mode/elisp")
                (require 'irony)
                (setq 
		 irony-server-executable
		 "/Users/Ron/.emacs.d/lisp/packages/irony-mode/bin/irony-server")
                (irony-enable 'ac)
                (irony-mode 1)))
  (message "irony-server is not compiled, please compile it."))

;; and python jedi completion
(eval-after-load "jedi"
  '(setq jedi:setup-keys t
         jedi:complete-on-dot t))
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (setq jedi:server-command
                  (list "/usr/local/bin/python" jedi:server-script))))

;; -----------------------------------------------------------------------------
;; GENERAL OPTIONS

;; rainbow mode
(add-hook 'prog-mode-hook 'rainbow-mode)

;; show matching braces
(show-paren-mode 1)
(setq show-paren-delay 0)

;; indent new lines
(electric-indent-mode 1)

;; update files when changed on disk
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; clean up the modeline
(autoload 'diminish "diminish")
(defun diminish-after-load (file mode)
  "After loading FILE, execute `diminish' on MODE."
  (eval-after-load file `(diminish ',mode)))
;; (mapc
;;  (lambda (x)
;;    (diminish-after-load (car x) (cdr x)))
;;  '(("eldoc"       . eldoc-mode)       ("rainbow-mode"  . rainbow-mode)
;;    ("hideshow"    . hs-minor-mode)    ("flyspell"      . flyspell-mode)
;;    ("undo-tree"   . undo-tree-mode)   ("whitespace"    . whitespace-mode)
;;    ("smartparens" . smartparens-mode) ("auto-complete" . auto-complete-mode)
;; ("abbrev" . abbrev-mode) ("volatile-highlights" . volatile-highlights-mode)))

;; disaster
(autoload 'disaster "disaster" nil t)
(eval-after-load "disaster"
  '(setq disaster-objdump
         "/usr/local/bin/gobjdump -d -M att -Sl --no-show-raw-insn"))

;; imenu-anywhere
(autoload 'imenu-anywhere "imenu-anywhere"
  "Switch to a buffer-local tag from Imenu via Ido." t)

;; ace-jump-mode
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/cache/saveplace")

;; savehist
(setq savehist-file "~/.emacs.d/cache/savehist")
(savehist-mode 1)

;; tramp
(eval-after-load "tramp-cache"
  '(setq tramp-persistency-file-name "~/.emacs.d/cache/tramp"))

;; smartparens mode
(require 'smartparens-config)
(smartparens-global-mode 1)

;; undo-tree mode
(require 'undo-tree)
(global-undo-tree-mode 1)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; extra dired commands
(require 'dired-x)

;; make dired use tar on OS X
(when (eq system-type 'darwin) (setq dired-guess-shell-gnutar "tar"))

;; clean up obsolete buffers at 00:00
(require 'midnight)

;; eldoc mode for emacs lisp...
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; ...for ielm...
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ...and for C
(autoload 'c-turn-on-eldoc-mode "c-eldoc" "Enable c-eldoc-mode" t)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; highlight lines longer than 80 characters
(autoload 'whitespace-mode "whitespace")
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; highlight pasted text
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; enable code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ediff
(eval-after-load "ediff"
  '(progn
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)
     (setq ediff-split-window-function 'split-window-horizontally)))

;; disable line wrapping in minibuffer too
(add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines t)))

;; indent c with four spaces
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; start the server if it isn't running
(require 'server)
(unless (and (daemonp) (server-running-p))
  (server-start))

(add-to-list 'load-path "~/.emacs.d/lisp/me")
;; TODO: autoload?
(require 'functions-lib)
(require 'functions)

;; -----------------------------------------------------------------------------
;; HOOKS

;; remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight a bunch of well known comment annotations
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; open binaries in hexl-mode or nhexl-mode when they're TAGS files
(add-hook 'find-file-hook
          (lambda ()
            (if (and (file-exists-p (buffer-file-name)) (> (buffer-size) 0))
                (if (and (file-binary-p (buffer-file-name))
                         (string= (buffer-local-value 'major-mode
                                                      (current-buffer))
                                  "fundamental-mode"))
                    (if (file-tags-naive-p (buffer-file-name))
                        (progn (require 'nhexl-mode)
                               (nhexl-mode))
                      (hexl-mode))))))

;; generate a tags file whenever you open a file
(add-hook 'prog-mode-hook
          (lambda ()
            (if (and (buffer-file-name)
                     (file-exists-p (buffer-file-name)))
                (progn
                  (setq temp-tags-command "env /usr/local/bin/ctags -e")
                  (function-list-for-file (buffer-file-name))
                  (temp-tags-file-for-file (buffer-file-name))))))

;; turn on `smartparens-mode' for `eval-expression'
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (if (eq this-command 'eval-expression)
                                       (smartparens-mode 1))))

;; regenerate tags whenever a function is added and the file is saved
(add-hook 'before-save-hook
          (lambda ()
            (if (not (eq (assoc (buffer-file-name) function-list-alist) nil))
                     (refresh-tags-for-file (buffer-file-name)))))

;; -----------------------------------------------------------------------------
;; KEY BINDINGS

;; make delete work like it does everywhere else
(global-set-key [delete]    'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; fix various keys
(define-key global-map [home]   'move-beginning-of-line)
(define-key global-map [end]    'move-end-of-line)
(define-key global-map [C-home] 'beginning-of-buffer)
(define-key global-map [C-end]  'end-of-buffer)
(define-key input-decode-map "\e[1;2D" [S-left])
(define-key input-decode-map "\e[1;2C" [S-right])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;2A" [S-up])

;; mouse events
(global-unset-key (kbd "M-<down-mouse-1>"))

(global-set-key (kbd "<mouse-2>")   'find-tag-by-event)
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; map functions
(global-set-key (kbd "C-d")           'duplicate-current-line-or-region)
(global-set-key (kbd "C-o")           'open-line-below)
(global-set-key (kbd "C-S-o")         'open-line-above)
(global-set-key (kbd "C-r")           'replace-char-at-point)
(global-set-key (kbd "C-S-s")         'isearch-backward)
(global-set-key (kbd "C-z")           'undo)
(global-set-key (kbd "C-+")           'text-scale-increase)
(global-set-key (kbd "C--")           'text-scale-decrease)
(global-set-key (kbd "C-`")           'auto-complete)
(global-set-key (kbd "C-<tab>")       'bury-buffer)
(global-set-key (kbd "C-c d")         'ediff-current-file)
(global-set-key (kbd "C-c e")         'eval-region)
(global-set-key (kbd "C-c E")         'eval-and-replace)
(global-set-key (kbd "C-c g")         'google-search)
(global-set-key (kbd "C-c i")        (lambda () (interactive)
                                       (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c h")         'hs-toggle-hiding)
(global-set-key (kbd "C-c m d")       'ff-find-other-file)
(global-set-key (kbd "C-c r")         'flyspell-correct-word-before-point)
(global-set-key (kbd "C-c w w")       'set-window-width)
(global-set-key (kbd "C-c w h")       'set-window-height)
(global-set-key (kbd "C-c SPC")       'ace-jump-mode)
(global-set-key (kbd "C-x f")         'recentf-ido-find-file)
(global-set-key (kbd "C-x g")         'imenu-anywhere)
(global-set-key (kbd "C-x p")         'toggle-window-dedicated)
(global-set-key (kbd "C-x m")         'eshell)
(global-set-key (kbd "C-x M")        (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x a f")       'find-file-at-point)
(global-set-key (kbd "C-x C-c")       'kill-frame-if-server)
(global-set-key (kbd "C-x C-k h")     'kill-help-buffer-and-window)
(global-set-key (kbd "C-x C-S-K")     'kill-buffer-and-window)
(global-set-key (kbd "C-x ! C-c")     'save-buffers-kill-emacs)
(global-set-key (kbd "C-x SPC")       'ace-jump-mode-pop-mark)
(global-set-key (kbd "M-g")           'goto-line)
(global-set-key (kbd "M-n")           'indent-region-or-buffer)
(global-set-key (kbd "M-;")           'comment-dwim-line)
(global-set-key (kbd "M-<up>")        'move-region-or-line-up)
(global-set-key (kbd "M-<down>")      'move-region-or-line-down)
(global-set-key (kbd "M-<kp-delete>") 'alt-delete-dwim)
(global-set-key (kbd "s-e")           'er/expand-region)
(global-set-key (kbd "s-r")           'repeat)
(global-set-key (kbd "C-M-\\")        'indent-region-or-buffer)
(global-set-key (kbd "C-M-<tab>")     'next-multiframe-window)
(global-set-key (kbd "C-S-z")         'undo-tree-redo)
(global-set-key (kbd "C-s-<left>")    'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>")   'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<up>")      'shrink-window)
(global-set-key (kbd "C-s-<down>")    'enlarge-window)
(global-set-key (kbd "C-M-s-<left>")  'shrink-frame-horizontally)
(global-set-key (kbd "C-M-s-<right>") 'enlarge-frame-horizontally)
(global-set-key (kbd "C-M-s-<up>")    'shrink-frame)
(global-set-key (kbd "C-M-s-<down>")  'enlarge-frame)
(global-set-key (kbd "M-s-<up>")     (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-s-<down>")   (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-M-z")         'indent-block)
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(global-set-key (kbd "<f1>")          'describe-mode)
(global-set-key (kbd "<f5>")         (lambda () (interactive)
                                       (load-file "~/.emacs.d/init.el")))

;; Map `C-x [0-3]' to `§-[0-3]'
(global-unset-key (kbd "§"))
(mapc (lambda (keyscommand)
        (global-set-key
         (kbd (concat "§ " (car keyscommand)))
         (cdr keyscommand)))
      '(("§" . (lambda () (interactive) (insert "§")))
        ("0" . delete-window)
        ("1" . delete-other-windows)
        ("2" . split-window-below)
        ("3" . split-window-right)
        ("h" . toggle-frame-max-height)
        ("m" . toggle-frame-max)
        ("t" . transparency)
        ("w" . toggle-frame-max-width)))

(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key [remap list-buffers]           'ibuffer-list-buffers)
(global-set-key [remap goto-line]              'goto-line-with-numbers)

;; -----------------------------------------------------------------------------
;; MODE SPECIFIC KEY BINDINGS

(defmacro define-key-after-load (file keymap key def)
  "After loading FILE, in KEYMAP, define key sequence KEY as DEF."
  `(eval-after-load ,file (quote (define-key ,keymap ,key ,def))))

(define-key-after-load "cc-mode" c-mode-base-map (kbd "s-i")   'llvm-gen-ir)
(define-key-after-load "cc-mode" c-mode-base-map (kbd "C-c a") 'disaster)

(define-key-after-load "cperl-mode" cperl-mode-map  (kbd "C-c p") 'cpan-search)
(define-key-after-load "python"     python-mode-map (kbd "C-c p") 'pypi-search)

(define-key-after-load "lisp-mode" emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)
(define-key-after-load "lisp-mode"
  lisp-mode-map (kbd "C-c C-n") 'ccl-insert-compile)
;; make saving eval the buffer in *scratch*`
(define-key-after-load "lisp-mode"
  lisp-interaction-mode-map [remap save-buffer] 'eval-buffer)

(define-key-after-load "slime-repl"
  slime-repl-mode-map (kbd "M-<up>")     'slime-repl-backward-input)
(define-key-after-load "slime-repl"
  slime-repl-mode-map (kbd "M-<down>")   'slime-repl-forward-input)
(define-key-after-load "slime-repl"
  slime-repl-mode-map (kbd "M-<return>") 'slime-repl-closing-return)

;; ielm
(define-key-after-load "comint"
  comint-mode-map (kbd "<M-down>") 'comint-next-input)
(define-key-after-load "comint"
  comint-mode-map (kbd "<M-up>")   'comint-previous-input)

;;; init.el ends 
