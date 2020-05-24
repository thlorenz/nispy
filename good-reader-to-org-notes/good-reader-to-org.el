;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-

(defconstant +HEADER+ "header")

(defun non-empty-lines (annotations)
  "Pull out non-empty ANNOTATIONS lines."
  (seq-filter (lambda (s) (not (string-empty-p s)))
              (split-string annotations "\n")))

(non-empty-lines "
  Highlight:
  Practical: A Simple Database")

(defun is-header-line (line)
  (if (string-match "^Highlight\:" line) t nil))

(defun is-item-line (line)
  (if (string-match "^Underline\:" line) t nil))

(let ((header nil))
  (when header (pp "header")))

(defun make-parse-entry (type data)
  (list :type type :data data))


(progn
  (setq lines '(
                "Highlight:"
                "Practical: A Simple Database"
                "Highlight:"
                "Other Practical: A Simple Database"
                ))
  (defun parse-annotation-lines (list)
    "Pull out header and item information from LIST."
    (let ((acc) (header-is-next nil) (item-is-next nil) (current-page))
      (nreverse
       (dolist
           (element list acc)
         (cond (header-is-next
                (progn
                  (push (make-parse-entry "header" element) acc)
                  (setq header-is-next nil)))
               ((is-header-line (car list))
                (setq header-is-next t)))
         ))))
  (parse-annotation-lines lines))



(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
