;;; good-reader-to-org.el --- Converts Good Reader Annotations to Org Mode Notes -*- lexical-binding: t; -*-
(require 'org)

(defconst *header+ "header")
(defconst *item+ "item")
(defconst *apple-script-template+ "
set posixFile to POSIX file \"%s\"
tell application \"Finder\" to open posixFile

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
          set value to (%s as text)
        end tell
        tell button \"OK\"
          click
        end tell
      end tell
    end tell
  end tell
end tell")

(defun nispy--scroll-osx-preview-app-to-page (pdf-file page)
  (do-applescript (format *apple-script-template+ pdf-file page)))

(defvar nispy--full-path-to-pdf)
(org-link-set-parameters
 "pdf"
 :follow
 (lambda (page)
   (nispy--scroll-osx-preview-app-to-page nispy--full-path-to-pdf page)))

(defun nispy--matches-any-p (regexes string)
  (if (position-if
       (lambda (regex) (string-match regex string))
       regexes)
      t nil))

(defun nispy--non-empty-lines (annotations)
  (seq-filter (lambda (s) (not (string-empty-p s)))
              (split-string annotations "\n")))

(defun nispy--header-line-p (line)
  (if (string-match "^Highlight\:" line) t nil))

(defun nispy--item-line-p (line)
  (if (string-match "^Underline\:" line) t nil))


(defun nispy--lisp-nispy--code-p (line)
  (nispy--matches-any-p '(
                   "^[;(]"
                   "^#'"
                   "^`("
                   "^'("
                   ) line ))

(defun nispy--algo-style-nispy--code-p (line)
  (if (string-match "(^[{}])" line) t nil))

(defun nispy--code-p (line)
  (or (nispy--lisp-nispy--code-p line)
      (nispy--algo-style-nispy--code-p line)))

(defun nispy--header-entry-p (entry)
  (eq (getf entry :type) *header+))

(defun nispy--make-parse-entry (type data page)
  (list :type type
        :data data
        :page page
        :is-code (and (equal *item+ type) (nispy--code-p data))))

(defun nispy--page-line-p (line)
  (if (string-match "--- Page \\([0-9]+\\) ---" line) t nil))

(defun nispy--parse-annotation-lines (list)
  "Pull out header and item information from LIST."
  (let (acc (header-is-next nil) (item-is-next nil) (current-page))
    (nreverse
     (dolist (element list acc)
       (cond
        (header-is-next
         (push (nispy--make-parse-entry *header+ element current-page) acc)
         (setq header-is-next nil))

        (item-is-next
         (progn
           (push (nispy--make-parse-entry *item+ element current-page) acc)
           (setq item-is-next nil)))

        ((nispy--header-line-p element)
         (setq header-is-next t))

        ((nispy--item-line-p element)
         (setq item-is-next t))

        ((nispy--page-line-p element)
         (setq current-page
               (string-to-number
                (substring element (match-beginning 1))))))))))

(defun nispy--map-entry-to-orgmode (page-offset entry)
  (let ((type (getf entry :type)))
    (cond ((eq type *header+)
           (list :type *header+
                 :content (format "* %s" (getf entry :data))))

          ((eq type *item+)
           (let ((code-indicator (if (getf entry :is-code) "~" "")))
             (list :type *item+
                   :content (format
                             "- [[pdf:%d][%d]] %s%s%s"
                             (getf entry :page)
                             (- (getf entry :page) page-offset)
                             code-indicator
                             (getf entry :data)
                             code-indicator)))))))

(defun nispy--map-entries-to-org-mode (list page-offset)
  (mapcar (lambda (element) (nispy--map-entry-to-orgmode page-offset element)) list))

(defun nispy--org-entries->string (entries)
  (reduce (lambda
            (acc entry)
            (if (nispy--header-entry-p entry)
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
    (setq lines (nispy--non-empty-lines original-string))
    (setq entries (nispy--parse-annotation-lines lines))
    (setq org-mode-entries (nispy--map-entries-to-org-mode entries page-offset))
    (setq rendered-string (nispy--org-entries->string org-mode-entries))
    (gui-set-selection 'CLIPBOARD rendered-string)
    (message "Formatted notes are in your clipboard.")))

(provide 'good-reader-to-org)
;;; good-reader-to-org.el ends here
