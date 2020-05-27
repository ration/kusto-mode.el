;;; kusto-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2019, by Hindol Adhya, Tatu Lahtela

;; Author: Hindol Adhya, Tatu Lahtela <tatu@lahtela.me>
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

(defconst kusto-data-types
  '("bool" "datetime" "dynamic" "guid" "int" "long" "real" "string" "timespan"
  "decimal"))

(defconst kusto-operators
  (let* ((x-tabular-operators
          '("as" "consume" "count" "datatable" "distinct" "evaluate" "extend"
            "externaldata" "facet" "find" "fork" "getschema" "invoke" "join"
            "limit" "lookup" "make-series" "mv-apply" "mv-expand" "order"
            "project" "project-away" "project-rename" "project-reorder" "parse"
            "parse-where" "partition" "print" "range" "reduce" "render" "sample"
            "sample-distinct" "search" "serialize" "sort" "summarize" "take"
            "top" "top-nested" "top-hitters" "union" "where"))
         (x-scalar-operators
          '("has" "has_cs" "hasprefix" "hasprefix_cs" "hassuffix" "hassuffix_cs"
            "contains" "contains_cs" "startswith" "startswith_cs" "endswith"
            "endswith_cs" "matches regex" "in" "in~" "has_any"))
         (x-logical-operators
          '("and" "or" "between")))
    (append x-tabular-operators
            x-scalar-operators
            x-logical-operators)))

(regexp-opt '("==" "!=" "=~" "!~" "<" ">" "<>" "and" "or" "between") 'symbols)

(defconst kusto-builtin-functions
  (let* ((x-binary-functions
          '("binary_and" "binary_not" "binary_or" "binary_shift_left"
            "binary_shift_right" "binary_xor" "bitset_count_ones"))
         (x-conversion-functions
          '("tobool" "todatetime" "todouble" "toreal" "tostring" "totimespan"))
         (x-time-functions
          '("ago" "datetime_add" "datetime_part" "datetime_diff" "dayofmonth"
            "dayofweek" "dayofyear" "endofday" "endofmonth" "endofweek"
            "endofyear" "format_datetime" "format_timespan" "getmonth" "getyear"
            "hourofday" "make_datetime" "make_timespan" "monthofyear" "now"
            "startofday" "startofmonth" "startofweek" "startofyear" "todatetime"
            "totimespan" "weekofyear"))
         (x-array-functions
          '("array_concat" "array_iif" "array_index_of" "array_length"
            "array_slice" "array_split" "bag_keys" "pack" "pack_all" "pack_array"
            "repeat" "set_difference" "set_has_element" "set_intersect"
            "set_union" "treepath" "zip"))
         (x-window-functions
          '("next" "prev" "row_cumsum" "row_number"))
         (x-flow-control-functions
          '("toscalar"))
         (x-mathematical-functions
          '("abs" "acos" "asin" "atan" "atan2" "beta_cdf" "beta_inv" "beta_pdf"
            "cos" "cot" "degrees" "exp" "exp10" "exp2" "gamma" "hash"
            "hash_combine" "hash_many" "isfinite" "isinf" "isnan" "log" "log10"
            "log2" "loggamma" "not" "pi" "pow" "radians" "rand" "range" "round"
            "sign" "sin" "sqrt" "tan" "welch_test"))
         (x-metadata-functions
          '("column_ifexists" "current_cluster_endpoint" "current_database"
            "current_principal" "current_principal_details"
            "current_principal_is_member_of" "cursor_after" "estimate_data_size"
            "extent_id" "extent_tags" "ingestion_time"))
         (x-rounding-functions
          '("bin" "bin_at" "ceiling" "floor"))
         (x-conditional-functions
          '("case" "coalesce" "iif" "iff" "max_of" "min_of"))
         (x-series-functions
          '("series_add" "series_divide" "series_equals" "series_greater"
            "series_greater_equals" "series_less" "series_less_equals"
            "series_multiply" "series_not_equals" "series_subtract"
            "series_decompose" "series_decompose_anomalies"
            "series_decompose_forecast" "series_fill_backward" "series_fill_const"
            "series_fill_forward" "series_fill_linear" "series_fir"
            "series_fit_2lines" "series_fit_2lines_dynamic" "series_fit_line"
            "series_fit_line_dynamic" "series_iir" "series_outliers"
            "series_pearson_correlation" "series_periods_detect"
            "series_periods_validate" "series_seasonal" "series_stats"
            "series_stats_dynamic"))
         (x-string-functions
          '("base64_encode_tostring" "base64_decode_tostring"
            "base64_decode_toarray" "countof" "extract" "extract_all"
            "extractjson" "indexof" "isempty" "isnotempty" "isnotnull" "isnull"
            "parse_csv" "parse_ipv4" "parse_json" "parse_url" "parse_urlquery"
            "parse_version" "replace" "reverse" "split" "strcat" "strcat_delim"
            "strcmp" "strlen" "strrep" "substring" "toupper" "translate" "trim"
            "trim_end" "trim_start" "url_decode" "url_encode"))
         (x-ipv4-functions
          '("ipv4_compare" "ipv4_is_match" "parse_ipv4" "parse_ipv4_mask"))
         (x-type-functions '("gettype"))
         (x-aggregation-functions
          '("dcount_hll" "hll_merge" "percentile_tdigest" "percentrank_tdigest"
            "rank_tdigest" "tdigest_merge"))
         (x-geospatial-functions
          '("geo_distance_2points" "geo_geohash_to_central_point"
            "geo_point_in_circle" "geo_point_to_geohash")))
    (append x-binary-functions
            x-conversion-functions
            x-time-functions
            x-array-functions
            x-window-functions
            x-flow-control-functions
            x-mathematical-functions
            x-metadata-functions
            x-rounding-functions
            x-conditional-functions
            x-series-functions
            x-string-functions
            x-ipv4-functions
            x-type-functions
            x-aggregation-functions
            x-geospatial-functions)))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq kusto-font-lock-keywords
      (let* ((x-keywords-regexp (regexp-opt kusto-operators 'symbols))
             (x-builtins-regexp (regexp-opt kusto-builtin-functions 'symbols))
             (x-datatypes-regexp (regexp-opt kusto-data-types 'symbols)))

        `((, "'[^']*'\\|\"[^\"]*\"" . font-lock-string-face)
          (, x-builtins-regexp . font-lock-builtin-face)
          (, x-datatypes-regexp . font-lock-builtin-face)
          (, x-keywords-regexp . font-lock-keyword-face)
          (, "\\<\\([[:digit:]]+\\)\\>" . font-lock-constant-face)
          (, "==\\|!=\\|=~\\|!~\\|<\\|>\\|<>\\|<=\\|>=" . font-lock-keyword-face)
          (, "!\\|~" . font-lock-negation-char-face)
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
(define-derived-mode kusto-mode text-mode "Kusto"
  "Major mode for editing Kusto Query Language"
  (make-local-variable 'kusto-indent-offset)
  (set (make-local-variable 'indent-line-function) 'kusto-indent-line)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((kusto-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'kusto-mode)

;;; kusto-mode.el ends here
