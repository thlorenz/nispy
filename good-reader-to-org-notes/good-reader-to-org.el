;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Thorsten Lorenz
;;
;;; Commentary:
;;; Code:

(defun is-string-empty (s)
  "Detect if S is an emtpy string."
  (string= "" s))

(let ((annotations "
line1

line2

line3
"))
  (seq-filter (lambda (s) (not (is-string-empty s)))
              (split-string annotations "\n")))

(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
