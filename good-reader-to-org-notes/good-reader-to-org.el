;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-

(defconst *header+ "header")
(defconst *item+ "item")
(defconst *apple-script-tempate+ "
set newPage to %d
tell application \"System Events\"
  tell process \"Preview\"
    set frontmost to true
    tell menu bar 1
      tell menu \"go\"
        click
        tell menu item \"Go to Page…\"
          click
        end tell
      end tell
    end tell
    tell window 1
      tell sheet 1
        tell text field 1
          set value to (newPage as text)
        end tell
        tell button \"OK\"
          click
        end tell
      end tell
    end tell
  end tell
end tell")

(defun nispy--scroll-osx-preview-app-to-page (page)
  (do-applescript (format *apple-script-tempate+ page)))

(defun non-empty-lines (annotations)
  (seq-filter (lambda (s) (not (string-empty-p s)))
              (split-string annotations "\n")))

(defun is-header-line (line)
  (if (string-match "^Highlight\:" line) t nil))

(defun is-item-line (line)
  (if (string-match "^Underline\:" line) t nil))

(defun has-lisp-code (line)
  (if (or (string-match "^[;(]" line)
          (string-match "^#'" line)
          (string-match "^`('" line)
          (string-match "^'('" line)
          ) t nil))

(defun has-algo-style-code (line)
  (if (string-match "(^[{}])" line) t nil))

(defun has-code (line)
  (or (has-lisp-code line)
      (has-algo-style-code line)))

(defun is-header-entry (entry)
  (eq (getf entry :type) *header+))

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
       (cond
        (header-is-next
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
                (substring element (match-beginning 1))))))))))

(defun map-entry-to-orgmode (page-offset entry)
  (let ((type (getf entry :type)))
    (cond ((eq type *header+)
           (list :type *header+
                 :content (format "* %s" (getf entry :data))))

          ((eq type *item+)
           (let ((code-indicator (if (getf entry :is-code) "~" "")))
             (list :type *item+
                   :content (format
                             "- [[elisp:(nispy--scroll-osx-preview-app-to-page %d)][%d]] %s%s%s"
                             (getf entry :page)
                             (- (getf entry :page) page-offset)
                             code-indicator
                             (getf entry :data)
                             code-indicator)))))))

(defun map-entries-to-org-mode (list page-offset)
  (mapcar (lambda (element) (map-entry-to-orgmode page-offset element)) list))

(defun org-entries->string (entries)
  (reduce (lambda
            (acc entry)
            (if (is-header-entry entry)
                (format "%s\n%s\n\n"
                        acc
                        (getf entry :content))
              (format "%s%s\n"
                      acc
                      (getf entry :content))))
          entries :initial-value ""))

(defun nispy-good-reader->org-mode (page-offset)
  (interactive "nEnter Page Offset: ")
  (let (original-string lines entries org-mode-entries rendered-string)
    (setq original-string (gui-get-selection 'CLIPBOARD))
    (setq lines (non-empty-lines original-string))
    (setq entries (parse-annotation-lines lines))
    (setq org-mode-entries (map-entries-to-org-mode entries page-offset))
    (setq rendered-string (org-entries->string org-mode-entries))
    (gui-set-selection 'CLIPBOARD rendered-string)
    (message "Formatted notes are in your clipboard.")))

(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
