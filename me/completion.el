;;; completion.el --- Completion-related configuration for Emacs.
;;
;; Copyright © 2013 Ron Mordechai
;;
;; Author: Ron Mordechai <ronmrdechai@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My Emacs configuration.  Geared towards C/C++, Python, ELisp, Perl and
;; assembly development.

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

;; make sure irony-server is compiled
(unless (file-executable-p ".emacs.d/vendor/irony-mode/bin/irony-server")
  (message "irony-server is not compiled, please compile it."))

;; activate autocomplete...
(require 'auto-complete-config)
(ac-config-default)
(setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
(ac-flyspell-workaround)

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'load-path
                         "/Users/Ron/.emacs.d/vendor/irony-mode/elisp")
            (require 'irony)
            (setq irony-server-executable
                  "/Users/Ron/.emacs.d/vendor/irony-mode/bin/irony-server")
            (irony-enable 'ac)
            (irony-mode 1)))
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)

;; and python jedi completion
(setq jedi:setup-keys t
      jedi:complete-on-dot t)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (setq jedi:server-command
                  (list "/usr/local/bin/python" jedi:server-script))))

(provide 'completion)
;;; completion.el end here
