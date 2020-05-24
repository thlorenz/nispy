;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Thorsten Lorenz
;;
;;; Commentary:
;;; Code:

(defun non-empty-lines (annotations)
  "Pull out non-empty ANNOTATIONS lines."
  (seq-filter (lambda (s) (not (string-empty-p s)))
              (split-string annotations "\n")))

(non-empty-lines "
  Highlight:
  Practical: A Simple Database")

(defun is-header-line (line)
  "Determine if LINE is a header line."
  (if (string-match "^Highlight\:" line) t nil))

(let ((header nil))
  (when header (pp "header")))

(setq lines '("Highlight:" "Practical: A Simple Database"))

;; TODO: dolist may work better here
(defun parse-annotation-lines (list)
  "Pull out header and item information from LIST."
  (let ((acc) (header-is-next nil) (item-is-next nil) (current-page))
    (while list
      (when header-is-next (push (car list) acc))
      (when (is-header-line (car list)) (setq header-is-next t))
      (setq list (cdr list)))
    acc))

(parse-annotation-lines lines)


(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
