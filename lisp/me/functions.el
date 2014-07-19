;;; functions.el --- interactive custom functions.
;;
;; Copyright © 2013 Ron Mordechai
;;
;; Author: Ron Mordechai <ronmrdechai@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A set of interactive functions to be bound to keys.
;; Load this file with (require 'functions)

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

(defmacro synthesize-search (name url prompt)
  "Synthesize an internet search method.  Its name be NAME + '-search'.
URL is concatenated to the user input, entered at PROMPT."
  `(defun ,(intern (format "%s-search" name)) ()
     ,(format "Search %s with a query or region if any." name)
     (interactive)
     (browse-url
      (concat ,url
              (url-hexify-string
               (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (read-string ,prompt nil nil
                              (thing-at-point 'word))))))))

(synthesize-search
 "google" "http://www.google.com/search?q="
 (let ((word (thing-at-point 'word)))
   (if word
       (format "Google (default %s): " word)
     "Google: ")))

(synthesize-search
 "cpan" "https://metacpan.org/search?q="
 (let ((word (thing-at-point 'word)))
   (if word
       (format "Search CPAN (default %s): " word)
     "Search CPAN: ")))

(synthesize-search
 "pypi" "https://pypi.python.org/pypi?%3Aaction=search&term="
 (let ((word (thing-at-point 'word)))
   (if word
       (format "Search PyPI (default %s): " word)
     "Search PyPI: ")))

(synthesize-search
 "youtube" "http://www.youtube.com/results?search_query="
 (let ((word (thing-at-point 'word)))
   (if word
       (format "Search YouTube (default %s): " word)
     "Search YouTube: ")))

(defun package-list-unaccounted-packages ()
  "Show packages that installed and are not in `my-packages'.
Useful for cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "s-j") 'top-join-line)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
Dedicated windows do not change when a new buffer is opened."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun llvm-gen-ir (&optional start end)
  "Convert the current buffer or region (containing C code) to LLVM IR.
Optionally, specify START and END to limit the conversion to certain area."
  (interactive)
  (let* ((start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max)))
         (major-mode 'llvm-mode)
         (buf (generate-new-buffer "*llvm-asm*"))
         (clang-exec "/usr/local/bin/clang-3.3")
         (opt-exec "/usr/local/bin/opt-3.3")
         (extention (file-name-extension buffer-file-name))
         (language (if (equal extention "c") "c" "c++")))
    (set-buffer-major-mode buf)
    (shell-command-on-region start end
                             (format "%s -emit-llvm -x %s -c -o - - |
 %s -S -mem2reg -basicaa -gvn" clang-exec extention opt-exec) buf)
    (set-buffer buf)
    (setq buffer-read-only t)
    (switch-to-buffer-other-window buf)))

(defun transparency (value)
  "Set the transparency of the frame window to VALUE."
  (interactive "NTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.
If no region is selected, the current line is not blank and
we are not at the end of the line, comment current line.
Optional argument ARG: if specified call `comment-kill'."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (progn
        (comment-or-uncomment-region
         (line-beginning-position) (line-end-position))
        (forward-line))
    (comment-dwim arg)))

(defun indent-block ()
  "Indent the current block."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
	(if (region-active-p)
		(progn
		  (indent-region (region-beginning) (region-end))
		  (message "Indented selected region."))
	  (progn
		(indent-region (point-min) (point-max))
		(message "Indented buffer.")))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
	(let ((line-move-visual nil))
	  (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun goto-line-with-numbers (&optional line)
  "Prompt the user for a line number, and jump to it.
Show line numbers temporarily while prompting for the line number input.  If
called with a prefix argument LINE, go to the line number without prompting."
  (interactive "^P")
  (if line
      (progn
        (goto-char (point-min))
        (forward-line (1- line)))
  (require 'linum)
  (let ((linum-status (if (and (boundp linum-mode) linum-mode) 1 -1)))
    (unwind-protect
        (progn
          (linum-mode 1)
          (call-interactively 'goto-line))
      (linum-mode linum-status)))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Find file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun kill-frame-if-server ()
  "Kill active frame, or kill Emacs if there is only no server running."
  (interactive)
  (let ((num (length (frame-list))))
    (if (daemonp)
        (if (> num 1)
            (delete-frame (selected-frame)))
      (save-buffers-kill-terminal))))

(defun move-region-or-line--internal (n)
  "Move the current line or region down N lines.
Moves the line up if N is negative."
  (let* ((start (point))
         (end start)
         col-init
         (col-end (current-column))
         exchange-pam
         delete-latest-newline)
    (when (region-active-p)
      (when (< (point) (mark))
        (setq exchange-pam t)
        (exchange-point-and-mark))
      (setq start (mark)
            end (point)
            col-end (current-column)))
    (goto-char start) (setq col-init (current-column))
    (beginning-of-line) (setq start (point))
    (goto-char end) (end-of-line)
    (when (= (point) (point-max))
      (setq delete-latest-newline t)
      (insert-char ?\n) (forward-char -1))
    (forward-char 1) (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (setq start (+ (point) col-init))
      (insert line-text))
    (forward-line -1)
    (forward-char col-end)
    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))
    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark start)
      (if exchange-pam
          (exchange-point-and-mark)))))

(defun move-region-or-line-up ()
  "Move the current line or region up one line."
  (interactive)
  (move-region-or-line--internal -1))

(defun move-region-or-line-down ()
  "Move the current line or region down one line."
  (interactive)
  (move-region-or-line--internal 1))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end)   (let (beg end)
                                   (if (and mark-active (> (point) (mark)))
                                       (exchange-point-and-mark))
                                   (setq beg (line-beginning-position))
                                   (if mark-active
                                       (exchange-point-and-mark))
                                   (setq end (line-end-position))
                                   (cons beg end)))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun eval-web-page ()
  "Download URL and evaluate its contents.
This function assumes URL contains valid elisp code."
  (interactive)
  (require 'url)
  (let ((url (read-string "Url: ")))
    (url-retrieve
     url
     (lambda (status url)
       (if (plist-get status :error)
           (error "Error accessing page.")
         ;; get rid of the HTTP request garbage
         (re-search-forward "^HTTP.*\n\\(.*\n\\)*?\n")
         (replace-match "" nil nil)
         (eval-buffer)
         (message (concat "Done evaluating web page: " url)))) (list url))))

(defun kill-whole-word ()
  "Kill whole word."
  (interactive)
  (if (looking-at "[a-zA-Z]")
      (save-excursion
        (while (looking-at "[a-zA-Z]") (backward-char))
        (forward-char)
        (kill-word 1)
        (if (eq (char-after) ? ) (just-one-space)))))

(defun enlarge-frame ()
  "Increase selected frame height by 1."
  (interactive)
  (set-frame-height (selected-frame) (+ (frame-height) 1)))
(defun shrink-frame ()
  "Decrease selected frame height by 1."
  (interactive)
  (set-frame-height (selected-frame) (- (frame-height) 1)))

(defun enlarge-frame-horizontally ()
  "Increase selected frame width by 1."
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width) 1)))
(defun shrink-frame-horizontally ()
  "Increase selected frame width by 1."
  (interactive)
  (set-frame-width (selected-frame) (- (frame-width) 1)))

(defun set-window-width (&optional width)
  "Set selected window's width to WIDTH."
  (interactive "NWidth: ")
  (adjust-window-trailing-edge (selected-window) (- width (window-width)) t))
(defun set-window-height (&optional height)
  "Set selected window's width to HEIGHT."
  (interactive "NHeight: ")
  (adjust-window-trailing-edge (selected-window) (- height (window-height))))

(defun alt-delete-dwim ()
  "Either delete word, or delete whitespace until next word."
  (interactive)
  (if (looking-at "[^\\\s]")
      (kill-word 1)
    (while (looking-at "[\\\s]")
      (delete-char 1))))

(defun kill-help-buffer-and-window ()
  "Kill the help buffer and delete its window."
  (interactive)
  (let* ((buffer (get-buffer "*Help*"))
         (window (get-buffer-window buffer)))
    (if (and buffer window)
        (progn
          (kill-buffer buffer)
          (delete-window window))
      (error "*Help* buffer not open"))))

(defadvice yank (around yank-and-indent activate)
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (if mark-active
      (delete-region (region-beginning) (region-end)))
  ad-do-it
  (call-interactively 'indent-region))

(defun find-tag-by-event (event)
  "Run `find-tag' for symbol at EVENT.
Bind this to a mouse button for IDE-like convenience."
  (interactive "@e")
  (if (nil-font-face-at-event-p event)
    (find-tag (prin1-to-string (symbol-at-event event)))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos
                       (cons (substring-no-properties name) position))))))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (or (equal major-mode 'dired-mode)
              (and (buffer-file-name)
                   (not (file-exists-p
                         (file-name-directory (buffer-file-name)))))
              (and (buffer-file-name)
                   (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun smooth-scroll-up (&optional arg)
  "Smoothly scroll ARG lines up.
If ARG is omitted scroll up a whole page.

Bind this to <prior>:
\(global-set-key (kbd \"<prior>\") 'smooth-scroll-up)"
  (interactive "^P")
  (condition-case nil
      (progn
        (let ((i 1)
              (amount (or arg (window-body-height))))
          (while (<= i amount)
            (scroll-down 1)
            (sit-for 0.01)
            (setq i (1+ i)))))
    (beginning-of-buffer
     (if arg (forward-line arg)
       (goto-char (point-min))))))

(defun smooth-scroll-down (&optional arg)
  "Smoothly scroll ARG lines down.
If ARG is omitted scroll down a whole page.

Bind this to <next>:
\(global-set-key (kbd \"<next>\") 'smooth-scroll-down)"
  (interactive "^P")
  (condition-case nil
      (progn
        (let ((i 1)
              (amount (or arg (window-body-height))))
          (while (<= i amount)
            (scroll-up 1)
            (sit-for 0.01)
            (setq i (1+ i)))))
    (end-of-buffer
     (if arg
         (forward-line arg)
       (goto-char (point-max))))))

(defun ccl-insert-compile ()
  "Insert CCL compile function at end of file."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n(save-application \"a.out\" "
            ":toplevel-function #'main :prepend-kernel t)")))

;; Vi-style functions
(defun open-line-above ()
  "Insert a new line above the current line, indent, put point at the begging."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun open-line-below ()
  "Insert a new line above the current line, indent, put point at the begging."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun replace-char-at-point ()
  "Replace character at point."
  (interactive)
  (let ((char (read-char)))
    (delete-char 1)
    (insert char)
    (backward-char 1)))

(defun delete-in-word ()
  "Delete word at point."
  (interactive)
  (backward-word 1)
  (kill-word 1))

(defun delete-in-quotes ()
  "Delete quoted text at point."
  (interactive)
  (let ((start (re-search-backward "'")))
    (forward-char 1)
    (kill-region start (re-search-forward "'"))
    (insert "''")
    (backward-char 1)))

(defun delete-in-double-quotes ()
  "Delete double-quoted text at point."
  (interactive)
  (let ((start (re-search-backward "\"")))
    (forward-char 1)
    (kill-region start (re-search-forward "\""))
    (insert "\"\"")
    (backward-char 1)))

(defun delete-in-parentheses ()
  "Delete text in parentheses at point."
  (interactive)
  (let ((start (re-search-backward "(")))
    (forward-char 1)
    (kill-region start (re-search-forward ")"))
    (insert "()")
    (backward-char 1)))

(defun delete-in-braces ()
  "Delete text in braces at point."
  (interactive)
  (let ((start (re-search-backward "{")))
    (forward-char 1)
    (kill-region start (re-search-forward "}"))
    (insert "{}")
    (backward-char 1)))

;; Frame maximization functions
(defun max-frame-width ()
  "Set frame width to maximum."
  (interactive)
  (if (not (frame-parameter (selected-frame) 'frame-at-max-width))
      (let* ((frame-pos-y (frame-parameter nil 'top))
             (max-cols (/ (display-pixel-width) (frame-char-width))))
        (set-frame-parameter (selected-frame) 'frame-restore-width
                             (frame-width))
        (set-frame-parameter (selected-frame) 'frame-restore-x
                             (frame-parameter nil 'left))
        (set-frame-parameter (selected-frame) 'frame-at-max-width t)
        (set-frame-width (selected-frame) max-cols)
        (set-frame-position (selected-frame) 0 frame-pos-y))))

(defun max-frame-height ()
  "Set frame height to maximum."
  (interactive)
  (if (not (frame-parameter (selected-frame) 'frame-at-max-height))
      (let* ((osx-menu-bar-height      ;; Height of OS X's menu bar +1 pixel
              (if (string-equal system-type "darwin") 23 0))
             (osx-title-bar-height 23) ;; Height of OS X's title bar
             (frame-pos-x (frame-parameter nil 'left))
             (max-rows (/ (- (display-pixel-height)
                             (+ osx-menu-bar-height
                                osx-title-bar-height))
                          (frame-char-height))))
        (set-frame-parameter (selected-frame) 'frame-restore-height
                             (frame-height))
        (set-frame-parameter (selected-frame) 'frame-restore-y
                             (frame-parameter nil 'top))
        (set-frame-parameter (selected-frame) 'frame-at-max-height t)
        (set-frame-height (selected-frame) max-rows)
        (set-frame-position (selected-frame) frame-pos-x osx-menu-bar-height))))

(defun restore-frame-width ()
  "Restore frame width after calling `max-frame-width'."
  (interactive)
  (if (frame-parameter (selected-frame) 'frame-at-max-width)
      (let ((frame-pos-y (frame-parameter nil 'top))
            (restore-width (frame-parameter (selected-frame)
                                            'frame-restore-width))
            (restore-x   (frame-parameter (selected-frame)
                                          'frame-restore-x)))
        (set-frame-parameter (selected-frame) 'frame-at-max-width nil)
        (set-frame-width (selected-frame) restore-width)
        (set-frame-position (selected-frame) restore-x frame-pos-y))))

(defun restore-frame-height ()
  "Restore frame height after calling `max-frame-height'."
  (interactive)
  (if (frame-parameter (selected-frame) 'frame-at-max-height)
      (let ((frame-pos-x (frame-parameter nil 'left))
            (restore-height (frame-parameter (selected-frame)
                                             'frame-restore-height))
            (restore-y    (frame-parameter (selected-frame)
                                           'frame-restore-y)))
        (set-frame-parameter (selected-frame) 'frame-at-max-height nil)
        (set-frame-height (selected-frame) restore-height)
        (set-frame-position (selected-frame) frame-pos-x restore-y))))

(defun toggle-frame-max-width ()
  "Set frame to maximum width or restore its width its previous state."
  (interactive)
  (if (frame-parameter (selected-frame) 'frame-at-max-width)
      (restore-frame-width)
    (max-frame-width)))

(defun toggle-frame-max-height ()
  "Set frame to maximum height or restore its height its previous state."
  (interactive)
  (if (frame-parameter (selected-frame) 'frame-at-max-height)
      (restore-frame-height)
    (max-frame-height)))

(defun toggle-frame-max ()
  "Set frame to maximum size or restore its size its previous state."
  (interactive)
  (if (and (frame-parameter (selected-frame) 'frame-at-max-width)
           (frame-parameter (selected-frame) 'frame-at-max-height))
      (progn
        (restore-frame-width)
        (restore-frame-height))
    (max-frame-width)
    (max-frame-height)))

(defun insert-init-el-title (name)
  "Insert `init.el' titles using NAME as the title name."
  (interactive "sName: ")
  (insert ";; -------------------------------------")
  (insert "----------------------------------------")
  (newline)
  (insert (format ";; %s OPTIONS" (upcase name)))
  (newline))

(defun insert-ip-address (num)
  "Insert IP address of device NUM at point."
  (interactive "P")
  (let* ((num (or num 0))
         (dev (concat (if (string-equal system-type "darwin")
                          "en" "eth")
                      (prin1-to-string num))))
    (insert (get-ip-address dev))))

(defadvice delete-indentation (before reverse-delete-indentation activate)
  "Reverse the way `delete-indentation' works."
  (ad-set-arg 0 (not (ad-get-arg 0))))

(provide 'functions)
;;; functions.el ends here
