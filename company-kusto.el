;;; company-kusto.el --- company-mode completion backend for kusto-mode  -*- lexical-binding: t -*-

;; Copyright © 2019, by Hindol Adhya

;; Author: Hindol Adhya <hindol.github.io>

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; In Emacs >= 26, company-capf is used instead.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'kusto-mode)

(defconst company-kusto-completions
  (append kusto-operators
          kusto-builtin-functions
          kusto-data-types))

;;;###autoload
(defun company-kusto (command &optional arg &rest ignored)
  "`company-mode' completion backend for `kusto-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-kusto))
    (prefix (and (eq major-mode 'kusto-mode)
                 (company-grab-symbol)))
    (candidates
     (cond
      ((company-grab-symbol)
       (all-completions arg company-kusto-completions))))
    (sorted t)))

(provide 'company-kusto)
;;; company-kusto.el ends here
