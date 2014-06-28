;;; jump-mode.el --- IDE-like jumping between function definitions.
;;
;; Copyright © 2013 Ron Mordechai
;;
;; Author: Ron Mordechai <ronmrdechai@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords: teleportation

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This mode abuses Emacs' tags feature to allow jumping between function
;; definitions.  A per-file tags file is generated for each file opened with
;; this mode, and it is updated whenever a function is added or removed from
;; the file.  Tags files are saved in your temporary directory and removed when
;; you quit Emacs.
;;
;; Symbols are prefixed with `jmp-'

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

(defun jmp-function-list-for-buffer ()
  "Generate a list of functions in buffer using `imenu'."
  (require 'cl-lib)
  (remove
   "*Rescan*"
   (remove
    "Variables"
    (cl-loop for (key) in (imenu--make-index-alist)
             collect key))))

(defun jmp-list-string= (list1 list2)
  "Return true if LIST1 and LIST2 contain the same elements.
This function disregards the order in which the elements appear."
  (require 'cl-lib)
  (cl-every 'string= (sort list1 'string<) (sort list2 'string<)))

(defvar jmp-file-function-alist nil
  "An associated list of all files and the functions they contain.")

(provide 'jump-mode)
;;; jump-mode.el ends here
