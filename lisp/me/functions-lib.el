;;; functions-lib.el --- Custom non-interactive functions.
;;
;; Copyright © 2013 Ron Mordechai
;;
;; Author: Ron Mordechai <ronmrdechai@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A set of non-interactive functions to be used in hooks.

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

(defun nil-font-face-at-event-p (event)
  "Return whether or not the face at EVENT is nil."
  (let* ((start-posn (event-start event))
         (start-point (posn-point start-posn)))
    (forward-char (- start-point (point)))
    (not (get-char-property (point) 'face))))

(defun symbol-at-event (event)
  "Move point to EVENT position and return `symbol-at-point'."
  (let* ((start-posn (event-start event))
         (start-point (posn-point start-posn)))
    (forward-char (- start-point (point)))
    (symbol-at-point)))

(defun get-ip-address (&optional dev)
  "Get the IP address for device DEV (default eth/en0)."
  (let ((dev (or dev
                 (concat (if (string-equal system-type "darwin")
                             "en" "eth") "0"))))
    (format-network-address (car (network-interface-info dev)) t)))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFRACTOR\\):"
          1 font-lock-constant-face t))))

(defun byte-plaintext-p (byte)
  "Determine whether BYTE is plaintext or not."
  (require 'cl-lib)
  (cl-case byte
    ((?\n ?\r ?\t) t)
    (otherwise (and (<= byte 126) (>= byte 32)))))
(defun file-binary-p (file)
  "Determine whether FILE is a binary file."
  (require 'cl-lib)
  (with-temp-buffer
    (insert-file-contents file)
    (cl-loop for c across
             (buffer-substring-no-properties
              1 (min 100 (buffer-size)))
             thereis (not (byte-plaintext-p c)))))
(defun file-tags-naive-p (file)
  "Determine whether FILE is a TAGS file."
  (let ((c (with-temp-buffer
             (insert-file-contents file nil 0 1) (buffer-string))))
    (if (= (aref c 0) 12) t nil)))


;; ---------
;; utilities

(defun util-list-string= (list1 list2)
  "Return true if LIST1 and LIST2 contain the same elements.
This function disregards the order in which the elements appear."
  (require 'cl-lib)
  (cl-every 'string= (sort list1 'string<) (sort list2 'string<)))


;; ------------------------------------------
;; TAGS and function construction and listing

(defvar function-list-alist nil
  "An alist of all the functions in each file.")

(defun add-to-function-list-alist (file function-list)
  "Add FILE and FUNCTION-LIST to `function-list-alist'."
  (add-to-list 'function-list-alist (cons file (list function-list))))

(defun function-list-for-file (file &optional noadd)
  "Generate a list of functions in FILE using `imenu'.
Don't add the list to `function-list-alist' if NOADD (defaults to t).  Opens and
switches to FILE."
  (require 'cl-lib)
  (require 'imenu)
  (find-file file)
  (let ((func-list (remove
                    "*Rescan*"
                    (remove
                     "Variables"
                     (cl-loop for (key) in (imenu--make-index-alist)
                              collect key)))))
    (if (not noadd) (add-to-function-list-alist file func-list))
    func-list))

(defvar temp-tags-file-alist nil
  "An alist of all files and their associated TAGS file.")

(defvar temp-tags-command "/usr/bin/ctags"
  "Command to run when generating a temporary TAGS file.")

(defun add-to-tags-file-alist (src-file tags-file)
  "Add SRC-FILE and TAGS-FILE to `temp-tags-file-alist'."
  (add-to-list 'temp-tags-file-alist (cons src-file tags-file)))

(defun temp-tags-file-for-file (file)
  "Generate a temporary tags file for FILE.
Add the file to the tags list and return the name of the file.

This function is non-blocking."
  (if (not (boundp 'temp-tags-command))
      (setq temp-tags-command "/usr/bin/ctags"))
  (let* ((temp-file (make-temp-file "EMACS_TAGS"))
         (proc (start-process-shell-command
                "temp-tags-proc"
                nil
                (format "%s -f %s %s" temp-tags-command temp-file file))))
    (set-process-sentinel proc
                          (lambda (proc msg)
                            (when (eq (process-status proc) 'exit)
                              (if (boundp 'temp-tags-file)
                                  (progn
                                    (add-to-list 'tags-table-list
                                                 temp-tags-file)
                                    (makunbound 'temp-tags-file))))))
    (add-to-tags-file-alist file temp-file)
    (setq temp-tags-file temp-file)
    temp-file))

(defun refresh-tags-for-file (file)
  "Check if a new function was added to FILE and generate a TAGS file for it."
  (let ((saved-funcs (cadr (assoc file function-list-alist)))
        (curr-funcs (function-list-for-file file t)))
    (if (not (util-list-string= saved-funcs curr-funcs))
        (progn
          (let ((tags-file (cdr (assoc file temp-tags-file-alist))))
            (setq function-list-alist
                  (remove (assoc file function-list-alist) function-list-alist))
            (function-list-for-file file)
            (setq temp-tags-file-alist
                  (remove
                   (assoc file temp-tags-file-alist) temp-tags-file-alist))
            (setq tags-table-list
                  (remove tags-file tags-table-list))
            (delete-file tags-file nil)
            (temp-tags-file-for-file file))))))

(defun temp-tags-file-for-dir (dir &optional tags-regexp)
  "Create a TAGS file for all files in DIR.
TAGS-REGEXP is the regular expression used to match the files, defaults to
using the buffer's file extension."
  (interactive)
  (let* ((extension (file-name-extension (buffer-file-name)))
         (temp-file (make-temp-file "EMACS_TAGS"))
         (regexp (if (boundp tags-regexp)
                     tags-regexp
                   (format ".*\.%s$" extension)))
         (command (format "find %s -regex \"%s\" | ctags -e -f %s -L-"
                           dir
                           regexp
                           temp-file))
         (proc (start-process-shell-command
                "temp-tags-proc"
                nil
                command)))
    (set-process-sentinel proc
                          (lambda (proc msg)
                            (when (eq (process-status proc) 'exit)
                              (if (boundp 'temp-tags-file)
                                  (progn
                                    (add-to-list 'tags-table-list
                                                 temp-tags-file)
                                    (makunbound 'temp-tags-file))))))
    (setq temp-tags-file temp-file)
    temp-file))


;; --------------------------
;; `Ispell' camelCase detection

(defun ispell-get-cC-word (following &optional extra-otherchars)
  "Return the word for spell-checking according to Ispell syntax.
If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding\) is checked when the cursor is not over a word.
Optional second argument contains otherchars that can be included in word
many times (see the doc string of `ispell-dictionary-alist' for details
about EXTRA-OTHERCHARS).

Word syntax is controlled by the definition of the chosen dictionary,
which is in `ispell-local-dictionary-alist' or `ispell-dictionary-alist'."
  (require 'ispell)
  (ispell-set-spellchecker-params)    ; Initialize variables and dicts alists
  (defmacro << (num1 val num2)
    "Determine if NUM1 < VAL < NUM2."
    `(and (< ,num1 ,val) (> ,num2 ,val)))
  (let* ((case-fold-search nil)       ; Make sure we search case sensitive
         (ispell-casechars (ispell-get-casechars))
         (ispell-not-casechars (ispell-get-not-casechars))
         (ispell-otherchars (ispell-get-otherchars))
         (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
         ;; "\\([A-Z]?[a-z]+\\('?[a-z]+\\)*\\|[A-Z]+\\('?[A-Z]+\\)*[a-z]?\\)"
         (word-regexp (concat "\\([A-Z]?[a-z]+\\("
                              (if (not (string= "" ispell-otherchars))
                                  (concat ispell-otherchars "?"))
                              (if extra-otherchars
                                  (concat extra-otherchars "?"))
                              "[a-z]+\\)"
                              (if (or ispell-many-otherchars-p
                                      extra-otherchars)
                                  "*" "?")
                              "\\|[A-Z]+\\("
                              (if (not (string= "" ispell-otherchars))
                                  (concat ispell-otherchars "?"))
                              (if extra-otherchars
                                  (concat extra-otherchars "?"))
                              "[A-Z]+\\)"
                              (if (or ispell-many-otherchars-p
                                      extra-otherchars)
                                  "*" "?")
                              "[a-z]?\\)"))
         did-it-once prevpt
         start end word)
    ;; look for the nearest word behind or in front
    (if (not (looking-at ispell-casechars))
        (if following
            (re-search-forward ispell-casechars (point-max) t)
          (re-search-backward ispell-casechars (point-min) t)))
    ;; move to front of word TODO: support otherchars
    (if (looking-at "[a-z]")
        (progn (while (looking-at "[a-z]")
                 (backward-char 1))
               (if (looking-at " ") (forward-char 1)))
      (let* ((chars-around (buffer-substring-no-properties (+ (point) -1)
                                                           (+ (point) 2)))
             (prevc (aref chars-around 0))
             (nextc (aref chars-around 2)))
        (if (and (<< 64 prevc 91) (<< 64 prevc 91))
            (progn (while (looking-at "[A-Z]")
                     (backward-char 1))
                   (forward-char 1)))))
    ;; Now mark the word and save to string.
    (if (not (re-search-forward word-regexp (point-max) t))
        (if ispell-check-only
            ;; return dummy word when just flagging
            (list "" (point) (point))
          (error "No word found to check!"))
      (setq start (copy-marker (match-beginning 0))
            end (point-marker)
            word (buffer-substring-no-properties start end))
      ;; make sure we match "FOO" in "FOOBar", and not "FOOB"
      (if (> (string-width word) 2)
          (if (and (<< 96 (aref (substring word -1) 0) 123)
                   (or (<< 64 (aref (substring word 1 2) 0) 91)
                       (= 32 (aref (substring word 0 1) 0))))
              (progn (setq word (substring word 0 -2)
                           end (- end 2))
                     (backward-char 2))))
      (list word start end))))


;; --------------------------
;; `Flyspell' textual menu

(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort (car (cdr (cdr poss))) 'string<)
                     (car (cdr (cdr poss)))))
         (cor-menu (if (consp corrects)
                       (mapcar (lambda (correct)
                                 (list correct correct))
                               corrects)
                     '()))
         (affix (car (cdr (cdr (cdr poss)))))
         show-affix-info
         (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                     (list
                                      (list (concat "Save affix: " (car affix))
                                            'save)
                                      '("Accept (session)" session)
                                      '("Accept (buffer)" buffer))
                                   '(("Save word" save)
                                     ("Accept (session)" session)
                                     ("Accept (buffer)" buffer)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu (mapcar
                (lambda (arg) (if (consp arg) (car arg) arg))
                base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))

(provide 'functions-lib)
;;; functions-lib.el ends here
