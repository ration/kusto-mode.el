;;; kusto-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2019, by Tatu Lahtela

;; Author: Tatu Lahtela <tatu@lahtela.me>
;; Version: 0.0.1
;; Created: 2019-09-12
;; Keywords: languages
;; Homepage: https://github.com/ration/kusto-mode.el

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Highlighting and indentation for Kusto https://docs.microsoft.com/en-us/azure/kusto/query/index

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq kusto-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("summarize" "where" "project" "order by" "let" "as"))
            (x-types '(""))
            (x-constants '(""))
            (x-events '(""))
            (x-functions '("count" "datetime" "toint" "tolong" "tostring" "any" "anyif" "arg_max" "arg_min" "avg" "avgif" "buildschema" "count" "countif" "dcount" "dcountif" "hll" "hll_merge" "make_bag" "make_list" "make_set" "max" "maxif" "min" "minif" "percentiles" "stdev" "stdevif" "stdevp" "sum" "sumif" "tdigest" "tdigest_merge" "variance" "varianceif" "variancep"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-events-regexp (regexp-opt x-events 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))


(defvar kusto-indent-offset 2
  "*Indentation offset for `kusto-mode'.")

(defun kusto-indent-line ()
  "Indent current line for `kusto-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[({]")
              (setq indent-col (+ indent-col kusto-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[)}]") (>= indent-col kusto-indent-offset))
        (setq indent-col (- indent-col kusto-indent-offset))))
    (indent-line-to indent-col)))

;;;###autoload
(define-derived-mode kusto-mode text-mode "kusto mode"
  "Major mode for editing Kusto Query Language"
  (make-local-variable 'kusto-indent-offset)
  (set (make-local-variable 'indent-line-function) 'kusto-indent-line)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((kusto-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'kusto-mode)

;;; kusto-mode.el ends here
