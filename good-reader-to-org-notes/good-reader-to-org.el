;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-

(defconst *header+ "header")
(defconst *item+ "item")

(defun non-empty-lines (annotations)
  "Pull out non-empty ANNOTATIONS lines."
  (seq-filter (lambda (s) (not (string-empty-p s)))
              (split-string annotations "\n")))

(defun is-header-line (line)
  (if (string-match "^Highlight\:" line) t nil))

(defun is-item-line (line)
  (if (string-match "^Underline\:" line) t nil))

(defun has-lisp-code (line)
  (if (string-match "^[(]" line) t nil))
(defun has-algo-style-code (line)
  (if (string-match "(^[{}])" line) t nil))

(defun has-code (line)
  (or (has-lisp-code line)
      (has-algo-style-code line)))

(defun make-parse-entry (type data)
  (list :type type
        :data data
        :is-code: (and (equal *item+ type) (has-code data))))

(pp
 (progn
   (defun parse-annotation-lines (list)
     "Pull out header and item information from LIST."
     (let (acc (header-is-next nil) (item-is-next nil) (current-page))
       (nreverse
        (dolist
            (element list acc)
          (cond (header-is-next
                 (push (make-parse-entry *header+ element) acc)
                 (setq header-is-next nil))

                (item-is-next
                 (progn
                   (push (make-parse-entry *item+ element) acc)
                   (setq item-is-next nil)))

                ((is-header-line element)
                 (setq header-is-next t))

                ((is-item-line element)
                 (setq item-is-next t))

                )))))
   (parse-annotation-lines lines)))


(setq lines (non-empty-lines "
Annotation summary:

--- Page 45 ---

Highlight:
Practical: A Simple Database


--- Page 46 ---

Underline:
(property list, or plist"))


(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
