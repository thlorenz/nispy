;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-

(defconst *header+ "header")
(defconst *item+ "item")

(defun non-empty-lines (annotations)
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

(defun make-parse-entry (type data page)
  (list :type type
        :data data
        :page page
        :is-code (and (equal *item+ type) (has-code data))))

(defun is-page-line (line)
  (if (string-match "--- Page \\([0-9]+\\) ---" line) t nil))

(defun parse-annotation-lines (list)
  "Pull out header and item information from LIST."
  (let (acc (header-is-next nil) (item-is-next nil) (current-page))
    (nreverse
     (dolist (element list acc)
       (cond (header-is-next
              (push (make-parse-entry *header+ element current-page) acc)
              (setq header-is-next nil))

             (item-is-next
              (progn
                (push (make-parse-entry *item+ element current-page) acc)
                (setq item-is-next nil)))

             ((is-header-line element)
              (setq header-is-next t))

             ((is-item-line element)
              (setq item-is-next t))

             ((is-page-line element)
              (setq current-page
                    (string-to-number
                     (substring element (match-beginning 1)))))

             )))))

(defun map-entry-to-orgmode (entry)
  (let ((type (getf entry :type)))
    (cond ((eq type *header+)
           (format "* %s" (getf entry :data)))

          ((eq type *item+)
           (let ((code-indicator (if (getf entry :is-code) "~" "")))
             (format "- %s%s%s"
                     code-indicator
                     (getf entry :data)
                     code-indicator
                     ))))))

;;; Test
(map-entry-to-orgmode item-entry)

(setq parsed-entries (parse-annotation-lines lines))
(setq header-entry
      (list :type *header+
            :data "Some header"
            :page 12
            :is-code nil))

(setq item-entry
      (list :type *item+
            :data "(setq a 1)"
            :page 111
            :is-code t))

(getf entry :type)

(defun map-entries-to-org-mode (list)
  (nreverse
   (let ((acc))
     (dolist (element list acc)
       (push (map-entry-to-orgmode element) acc)))))

(map-entries-to-org-mode parsed-entries)

(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
