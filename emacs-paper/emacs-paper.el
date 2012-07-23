;;; Emacs Paper --- An Emacs mode for managing and browsing references.

;; Copyright 2009-2011 Olof Ohlsson Sax.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, see 
;; <http://www.gnu.org/licenses/>.

;;; Set up variables
(defcustom ep-main-bib-file ""
  "Main BibTeX database used by Emacs Paper."
  :type 'string)

(defcustom ep-pdf-dir nil
  "Directory for storing PDF-files."
  :type '(restricted-sexp :match-alternatives (stringp 'nil)))

(defcustom ep-temp-dir "/tmp/"
  "Directory for storing temporary files.")

(defcustom ep-pdf-file nil
  "File for storing the list of PDF-files."
  :type '(restricted-sexp :match-alternatives (stringp 'nil)))

(defcustom ep-add-pdf-to-itunes-script nil
  "Path to AppleScript to add papers to iTunes."
  :type '(restricted-sexp :match-alternatives (stringp 'nil)))

(defcustom ep-add-pdf-to-itunes-automatically nil
  "Automatically add PDF files to iTunes"
  :type 'boolean)

(defvar ep-fix-titles 't
  "*Should titles be 'fixed'?")

(defcustom ep-arxiv-default-category "hep-th"
  "Default arXiv category to use when checking for new articles."
  :type 'string)

(defcustom ep-highlight-color "honeydew1"
  "Background color of highlighted entry."
  :type 'color)

(defcustom ep-spires-url 
  "http://www.slac.stanford.edu/spires/find/hep/www?rawcmd="
  "Base URL used for Spires queries."
  :type 'string)

(defcustom ep-inspire-url 
  "http://inspirehep.net/search?p="
  "Base URL used for Inspire queries."
  :type 'string)

(defcustom ep-arxiv-url "http://arxiv.org"
  "Base URL for visiting the arXiv."
  :type 'string)

(defcustom ep-arxiv-api-url "http://export.arxiv.org/api/query"
  "Base URL for arXiv API."
  :type 'string)

(defcustom ep-arxiv-rss-url "http://export.arxiv.org/rss/"
  "Base URL for arXiv RSS."
  :type 'string)

(defcustom ep-message-cmd nil
  "Command called to send messages.")

(defcustom ep-open-pdf-cmd "open \"%s\""
  "Command to call for opening PDF files.
The final command will be formated using 'format' and should
include a single '%s' which will be substituted with the
filename."
  :type 'string)

(defcustom ep-enable-save-hook t
  "Search for visiting Emacs Paper buffers when saving a BibTeX file."
  :type 'boolean)

(defvar ep-main-buffer nil
  "Main Emacs Paper buffer")

(defvar ep-bib-fields
  '("author" "title" "journal" "volume" "number" "publisher" "year" "month" 
    "edition" "address" "pages" "eprint" "archivePrefix" "primaryClass"
    "doi" "school" "series" "SLACcitation" "note" "ep-tags")
  "BibTeX fields saved by Emacs Paper. The fields are inserted in
  the order of appearance in the list.")

(defvar ep-ep-common-tags '("Printed" "TODO: Print")
  "List of common Emacs Paper tags, used for completion when
  adding a tag to an entry.")

(defvar ep-ep-highlight-overlay nil
  "Overlay used to highlight the current entry.")

(defvar ep-ep-current-entry nil
  "The current active entry in the buffer.")

(defvar ep-ep-visited-file nil
  "If the current Emacs Paper buffer is visiting a file, this
  variable contains the filename.")

(defvar ep-ep-file-buffers nil
  "List of all Emacs Paper buffer visiting files.")

(defvar ep-pdf-list '(nil)
  "Mapping between BibTeX keys and PDF files.")

;;; General helper functions 

(defmacro ep-ep-message (fmt &rest args)
  `(progn
     (when ep-message-cmd
       (shell-command (concat ep-message-cmd "\"" (format ,fmt ,@args) "\"")))
     (message ,fmt ,@args)))

(defun ep-ep-cleanup-whitespace (string)
  "Remove any tabs, newlines and double spaces from STRING."
  (when string
    (while (string-match "[\n\t]+" string)
      (setq string (replace-match " " nil t string)))
    (while (string-match "  +" string)
      (setq string (replace-match " " nil t string))))
  string)

(defun ep-ep-string-match-full (regexp string)
  "Check if 'string' exactly matches 'regexp'"
  (and (string-match regexp string)
	   (equal (match-beginning 0) 0)
	   (equal (match-end 0) (length string))))

(defmacro ep-ep-alist-insert (key alist val)
  "Insert (KEY . VAL) into alist, unless there already is an
entry for KEY.
WARNING: evaluates the parameters more than once! Fix this!"
  `(let ((local-key ,key)
         (local-val ,val))
     (unless (ep-ep-alist-get-value local-key ,alist)
       (if (not (assoc local-key ,alist))
           (if ,alist
               (setcdr (last ,alist) (list (cons local-key local-val)))
             (setq ,alist (list (cons local-key local-val))))
         (setcdr (assoc local-key ,alist) local-val))
       (list (cons local-key local-val)))))

(defmacro ep-ep-alist-set (key alist val)
  "Set the value of KEY in ALIST to VAL. Add the entry if it does not exist.
WARNING: evaluates the parameters more than once! Fix this!"
  `(let* ((local-key ,key)
          (local-val ,val)
          (local-field (assoc local-key ,alist)))
    (if local-field
         (setcdr local-field local-val)
      (ep-ep-alist-insert local-key ,alist local-val))))

(defun ep-ep-alist-get-value (key alist)
  "Get the value of KEY in ALIST."
  (cdr (assoc key alist)))

(defun ep-ep-alist-clear (alist)
  "Set all values in ALIST to nil."
  (dolist (field alist)
    (setcdr field nil)))

(defun ep-ep-replace-regexp (regexp string)
  "Replaces any string matching 'regexp' with 'string'"
  (goto-char (point-min))
  (let ((case-fold-search nil))
	(while (re-search-forward regexp nil t) 
	  (replace-match string t nil))))

(defun ep-ep-string-match-full (regexp string)
  "Check if 'string' exactly matches 'regexp'"
  (and (string-match regexp string)
	   (equal (match-beginning 0) 0)
	   (equal (match-end 0) (length string))))


(defun ep-ep-pop-from-file-name-history (filename)
  "Remove 'filename' from the top of 'file-name-history'."
  (when (string-equal (car file-name-history) filename)
    (setq file-name-history (cdr file-name-history))))

;;; Entry helper functions

(defun ep-ep-field-value (field-name entry)
  "Return the value of FIELD in ENTRY if set, or nil."
  (cdr (assoc-string field-name entry)))

;;; Loading and saving BibTeX files

(defun ep-bib-load-file (file)
  "Load a BibTex FILE and display it in a new Emacs Paper buffer."
  (interactive "FLoad BibTeX file: ")

  (let* ((file-buf (find-file file))
         (entries (ep-bib-parse-buffer file-buf)))
    (if (not entries)
        (message "No BibTeX entries found in %s" file)

    ;; Sort the entries so that 'Preamble' and 'String' entries are at the top
      (setq entries (funcall (ep-ep-sort-function "==unused==") entries))

      (ep-ep-new-buffer (concat "EP:" (file-name-nondirectory file))
        (ep-ep-insert-main-heading (concat "Emacs Paper -- " (file-name-nondirectory file)))
        (ep-ep-format-entries entries))
      (set (make-variable-buffer-local 'ep-ep-visited-file) file)

      (unless (member 'ep-ep-kill-buffer-query-function kill-buffer-query-functions)
        (push 'ep-ep-kill-buffer-query-function kill-buffer-query-functions))

      (push (current-buffer) ep-ep-file-buffers)
      (unless (member 'ep-ep-kill-emacs-query-function kill-emacs-query-functions)
        (push 'ep-ep-kill-emacs-query-function kill-emacs-query-functions)))
    (kill-buffer file-buf)

    (current-buffer)))

(defun ep-bib-save-file (&optional new-name buffer)
  "Save the entries in the Emacs Paper BUFFER, or the
`current-buffer', to file as BibTeX entries."
  (interactive "P")

  (when buffer (set-buffer buffer))

  (if (and (not new-name) ep-ep-visited-file (not (buffer-modified-p)))
      (message "(No changes need to be saved)")
    (let ((entries (ep-ep-extract-entries))
          no-key)
      (while (and entries (not no-key))
        (unless (or (ep-ep-field-value "=key=" (car entries)) 
                    (string-equal (ep-ep-field-value "=type=" (car entries)) "Preamble")
                    (string-equal (ep-ep-field-value "=type=" (car entries)) "String"))
          (setq no-key (car entries)))
        (pop entries))
      (when no-key
        (goto-char (car (ep-ep-entry-boundaries no-key)))
        (error "Entry has no key")))

    (save-excursion
      (let ((filename (or (and (not new-name) ep-ep-visited-file) (read-file-name "Save to file:")))
            (entries (ep-ep-extract-entries)))
        (cond
         ((not filename) (error "Empty filename"))
         ((not entries) (error "The buffer contain no entries"))
         (t
          (ep-bib-save-entries entries filename)
          (message "%d entries saved to %s" (length entries) filename)
          (setq ep-ep-visited-file filename)
          
          (goto-char (point-min))
          
          (when (get-text-property (point) :ep-heading)
            (toggle-read-only -1)
            (delete-region (point) (next-single-property-change (point) :ep-heading))
            (ep-ep-insert-main-heading (concat "Emacs Paper -- " (file-name-nondirectory ep-ep-visited-file)))
            (toggle-read-only +1))
          (set-buffer-modified-p nil)))))))

(defun ep-ep-kill-buffer-query-function ()
  "Check if the current EP buffer is visiting a file and is
modified, then ask the user wether the buffer should be
killed. Called from `kill-buffer'."
  (if (or (not ep-ep-visited-file) (not (buffer-modified-p)))
      t
    (yes-or-no-p (concat "Buffer " (buffer-name (current-buffer)) " modified; kill anyway? "))))

(defun ep-ep-kill-emacs-query-function ()
  "Check if there are any unsaved buffers, and ask if they should
be saved. If not, ask wether we still should exit Emacs. Called
from `save-buffers-kill-terminal'."
  (let ((all-saved t))
    (dolist (buffer ep-ep-file-buffers)
      (when (and (buffer-live-p buffer) (buffer-modified-p buffer))
        (if (y-or-n-p (format "Save EP buffer %s " (buffer-name buffer) "? "))
            (ep-bib-save-file nil buffer)
          (setq all-saved nil))))
    (if all-saved
        t
    (yes-or-no-p "Modified Emacs Paper buffer exist; exit anyway?"))))

(defun ep-bib-parse-buffer (buffer)
  "Parse all BibTeX entries in BUFFER. Return a list of the
parsed entries."
  (save-current-buffer
    (set-buffer buffer)
    (let ((progress (make-progress-reporter "Parsing entries..." (point-min) (point-max)))
          entries)
      (save-excursion
        (goto-char (point-min))
        
        (while (re-search-forward "^@" nil t)
          (backward-char)
          (cond
           ;; Preamble entry
           ((looking-at bibtex-preamble-prefix)
            (let* ((parse-result (bibtex-parse-preamble))
                   (bounds (cdr parse-result))
                   (entry-end )
                   (preamble-string (buffer-substring-no-properties (car bounds) (cadr bounds)))
                   entry)
              (ep-ep-alist-insert "=type=" entry "Preamble")
              (ep-ep-alist-insert "=content=" entry preamble-string)
              (push entry entries)
              (goto-char (caddr bounds))
              (progress-reporter-update progress (point))))
           ;; String entry
           ((looking-at bibtex-string-type)
            (let* ((parse-result (bibtex-parse-string))
                   (key-bounds (car parse-result))
                   (key (buffer-substring-no-properties (cadr key-bounds) (caddr key-bounds)))
                   (string-bounds (cdr parse-result))
                   (string (buffer-substring-no-properties (car string-bounds) (cadr string-bounds)))
                   (entry-end (caddr string-bounds))
                   entry)
              (ep-ep-alist-insert "=type=" entry "String")
              (ep-ep-alist-insert "=content=" entry (concat key " = " string))
              (goto-char entry-end)
              (push entry entries)
            ))
           (t
            (let* ((entry (bibtex-parse-entry t)))
              (dolist (field entry)
                (setcdr field (ep-ep-cleanup-whitespace (cdr field))))
              (push entry entries))
            (progress-reporter-update progress (point)))))
        (progress-reporter-done progress)
        (nreverse entries)))))

(defun ep-bib-save-entries (entries file)
  "Save the BibTeX ENTRIES to FILE."
  (save-current-buffer
    (find-file file)
    (ep-bib-format-entries entries)
    (let ((ep-enable-save-hook nil))
      (save-buffer))
    (kill-buffer (current-buffer))))

(defun ep-bib-format-entries (entries)
  "Write ENTRIES as BibTeX entries to current buffer.

Warning: the buffer will be erased before the entries are
inserted!

The file is not saved."
  (let ((did-write nil))
    (widen)
    (unless (and (buffer-modified-p)
                 (not (y-or-n-p (concat "File " (buffer-name) " has been changed."
                                        " Inserting entries will overwrite these changes."
                                        " Do you want to continue? "))))
      (erase-buffer)
      (insert "This file was created by Emacs Paper.\n\n")
      (let ((progress (make-progress-reporter "Insering entries..." 0 (length entries)))
	    (counter 0))
	(dolist (entry entries)
	  (ep-bib-format-entry entry)
          (incf counter)
	  (progress-reporter-update progress counter)
	  (insert "\n\n"))
	(progress-reporter-done progress))
      (setq did-write t))
    did-write))

(defun ep-bib-format-entry (entry)
  "Insert ENTRY in current buffer."

  (insert "@")
  (ep-ep-insert-non-nil (ep-ep-field-value "=type=" entry))
  (insert (bibtex-entry-left-delimiter))
  (ep-ep-insert-non-nil (ep-ep-field-value "=key=" entry))

  (if (ep-ep-alist-get-value "=content=" entry)
      (insert (ep-ep-alist-get-value "=content=" entry))
    (dolist (field-name ep-bib-fields)
      (let ((field-value (ep-ep-field-value field-name entry)))
        (if field-value
            (bibtex-make-field (list field-name nil field-value nil)))))
    (insert "\n"))
  (insert (bibtex-entry-right-delimiter)))

(defun ep-bib-find-file ()
  "Go to the BiBTeX-file visited by the Emacs Paper buffer."
  (interactive)
  (if (not ep-ep-visited-file)
      (message "The current buffer is not visiting a BibTeX file")
    (when (and (buffer-modified-p) (y-or-n-p (concat "Save EP buffer " (buffer-name) " to " ep-ep-visited-file " before visiting? ")))
      (ep-bib-save-file))
    (let ((key (ep-ep-field-value "=key=" ep-ep-current-entry)))
      (find-file ep-ep-visited-file)
      (goto-char (point-min))
      (search-forward (concat "{" key))
      (beginning-of-line)
      (add-hook 'after-save-hook 'ep-ep-bib-after-save-hook))))

(defun ep-ep-bib-after-save-hook ()
  "Check if there are any visiting Emacs Paper buffers when saving a BibTeX file."
  (save-excursion
    (when (and ep-enable-save-hook
               (equal major-mode 'bibtex-mode)) ;; Only check when saving a BibTeX buffer

      (let ((saved-file-name (expand-file-name (buffer-file-name)))
            ep-buffer)

        ;; Is it the main Emacs Paper buffer that is being changed?
        (if (string-equal saved-file-name (expand-file-name ep-main-bib-file))
            (when (y-or-n-p (concat "Main Emacs Paper BibTeX database changed. Reload Emacs Paper main buffer (this will close " saved-file-name ")? "))
              ;; Close and reopen the main buffer
              (kill-buffer ep-main-buffer)
              (ep-main))

          ;; Search for Emacs Papers visiting the newly saved file. Note that we actually only find the LAST buffer visiting the file...
          (dolist (buf (buffer-list))
            (set-buffer buf)
            (when (and (equal major-mode 'ep-ep-mode)
                       (string-equal (expand-file-name ep-ep-visited-file) 
                                     saved-file-name))
              (setq ep-buffer (current-buffer))))
          (when (and ep-buffer
                     (y-or-n-p (concat "BibTeX file visited by Emacs Paper buffer " (buffer-name ep-buffer) " changed. "
                                       "Reload Emacs Paper buffer (this will close " saved-file-name ")? " )))
            ;; Close and reopen the visiting buffer
            (kill-buffer ep-buffer)
            (ep-bib-load-file saved-file-name)))
        ;; TODO: find the edited entry!
        ))))


;;; EP buffer formatting

;; Some formatting helper macros

(defmacro ep-ep-insert-non-nil (&rest args)
  "Insert ARGS into buffer unless any of the arguments are
nil. Return t if anything was inserted, otherwise nil."
  `(if (member nil (list ,@args))
       nil
     (insert ,@args)
     t))

(defmacro ep-ep-propertize-non-nil (string &rest properties)
  "Act as `propertize', but return nil if STRING is nil."
  `(if ,string
       (propertize ,string ,@properties)
     nil))

(defmacro ep-ep-concat-non-nil (&rest args)
  "Act as concat, but return nil if any of the arguments are nil."
  `(if (member nil (list ,@args))
       nil
      (concat ,@args)))

(defmacro ep-ep-substring-non-nil (string from &optional to)
  "Act as `substring', but return nil if STRING is nil"
  `(if ,string
       (substring ,string ,from ,to)
     nil))

;; Entry formatting

(defun ep-ep-format-entry (entry)
  "Insert ENTRY nicely fromatted into current buffer."
  (insert "\n")
  (let* ((start (point)))
    (cond 
     ((string-equal (ep-ep-field-value "=type=" entry) "Preamble")
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-ep-field-value "=content=" entry)
                                                      'face '(:slant italic :height 0.8)) "\n"))
     ((string-equal (ep-ep-field-value "=type=" entry) "String")
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-ep-field-value "=content=" entry)
                                                      'face '(:slant italic))))
     (t
      (or (ep-ep-insert-non-nil (ep-ep-field-value "=key=" entry))
          (ep-ep-insert-non-nil (ep-ep-field-value "eprint" entry)  " "
                                "[" (ep-ep-field-value "primaryClass" entry) "]")
          (ep-ep-insert-non-nil (ep-ep-field-value "eprint" entry)))

      (cond
       ((and (ep-ep-field-value "=ep-updated=" entry) (ep-ep-field-value "=ep-cross-list=" entry))
        (insert " (cross-list, updated)"))
       ((ep-ep-field-value "=ep-updated=" entry)
        (insert " (updated)"))
       ((ep-ep-field-value "=ep-cross-list=" entry)
        (insert " (cross-list)")))

      (when (and (ep-ep-field-value "=key=" entry) ep-pdf-list (ep-ep-alist-get-value (ep-ep-field-value "=key=" entry) ep-pdf-list))
        (insert " (PDF)"))

      (insert "\n")
      
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil 
                             (ep-ep-field-value "title" entry) 
                             'face '(:weight bold :slant italic :height 1.1)) "\n")
      (ep-ep-insert-non-nil (ep-ep-field-value "author" entry))

      (when (or (ep-ep-field-value "year" entry) 
                (ep-ep-field-value "journal" entry)
                (and (ep-ep-field-value "=key=" entry) (ep-ep-field-value "eprint" entry)))
        (insert "\n"))
    
      ;; (if (string-equal "JHEP" (ep-ep-field-value "journal" entry))
      ;;     (ep-ep-insert-non-nil (ep-ep-propertize-non-nil 
      ;;                            (ep-ep-field-value "journal" entry) 'face 'italic ) " " 
      ;;                            (ep-ep-propertize-non-nil 
      ;;                             (ep-ep-concat-non-nil 
      ;;                              (ep-ep-substring-non-nil (ep-ep-field-value "year" entry) 2)
      ;;                              (ep-ep-field-value "volume" entry))
      ;;                             'face 'bold) ", "
      ;;                             (ep-ep-field-value "pages" entry) " ")
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-ep-field-value "journal" entry) 
                                                      'face 'italic ) " "
                                                      (ep-ep-propertize-non-nil (ep-ep-field-value "volume" entry) 
                                                                                'face 'bold) ", "
                                                                                (ep-ep-field-value "pages" entry) " ")
      (ep-ep-insert-non-nil "(" (ep-ep-field-value "year" entry) ")")
      
      (when (ep-ep-field-value "=key=" entry)
        (when (and (or (ep-ep-field-value "journal" entry)
                       (ep-ep-field-value "year" entry))
                   (ep-ep-field-value "eprint" entry))
          (insert ", "))
        (or (ep-ep-insert-non-nil (ep-ep-field-value "eprint" entry)  " "
                                  "[" (ep-ep-field-value "primaryClass" entry) "]")
            (ep-ep-insert-non-nil (ep-ep-field-value "eprint" entry))))
      (insert ".\n")
      (ep-ep-insert-non-nil "Tags: " (ep-ep-field-value "ep-tags" entry) "\n")
      
      (when (ep-ep-field-value "abstract" entry)
        (ep-ep-insert-non-nil "Comments: " (ep-ep-field-value "arxiv-comment" entry) "\n")
        (insert "\n")
        (insert (ep-ep-field-value "abstract" entry)))
      
      (ep-ep-insert-non-nil "Note: " (ep-ep-field-value "note" entry) "\n")))
     
     (put-text-property start (point) :ep-entry entry)))

(defun ep-ep-format-entries (entries)
  "Insert ENTRIES in current buffer."
  (let ((seen-preamble nil)
        (seen-string nil)
        (seen-other nil))
    (dolist (entry entries)
      (let ((entry-type (ep-ep-alist-get-value "=type=" entry)))
        (cond 
         ((string-equal entry-type "Preamble")
          (when (not seen-preamble)
            (setq seen-preamble t)
            (insert "\n" (propertize "Preamble" 'face '(:weight bold :height 1.1 :underline t)) "\n")))
         ((string-equal entry-type "String")
          (when (not seen-string) 
            (setq seen-string t)
            (insert "\n" (propertize "Strings" 'face '(:weight bold :height 1.1 :underline t)) "\n")))
         (t
          (when (and (not seen-other) (or seen-preamble seen-string))
            (setq seen-other t)
            (when seen-string
              (insert "\n"))
            (insert "\n" (propertize "References" 'face '(:weight bold :height 1.1 :underline t)) "\n"))))
        (ep-ep-format-entry entry)))))

(defun ep-ep-insert-main-heading (heading)
  "Insert HEADING in current buffer."
  (let ((start (point)))
    (unless (bobp)
      (insert "\n"))
    (insert (propertize heading  'face '(:weight bold :height 1.5 :underline t)))
    (insert "\n")
    (put-text-property start (point) :ep-heading t)))

(defun ep-ep-insert-sub-heading (heading)
  "Insert HEADING in current buffer."
  (let ((start (point)))
    (unless (bobp)
      (insert "\n"))
    (insert (propertize heading 'face '(:weight bold :height 1.2 :underline t)))
    (insert "\n")
    (put-text-property start (point) :ep-heading t)))

;;; EP buffer navigation

(defun ep-ep-next-entry ()
  "Move point to the beginning of the next entry and return
`point'. If the current entry is the last one in the buffer,
leave `point' unchanged and return nil."
  (let* ((next (next-single-property-change (point) :ep-entry))
         (next-to-next (when next (next-single-property-change next :ep-entry))))
    (cond
     ((and next (ep-ep-entry-at-point next))
      (goto-char next) 
      (point))
     ((and next-to-next (ep-ep-entry-at-point next-to-next))
      (goto-char next-to-next)
      (point))
     (t nil))))

(defun ep-ep-previous-entry ()
  "Move point to the beginning of the previous entry and return
`point'. If the current entry is the first one in the buffer,
leave `point' unchanged and return nil."
  (let* ((prev (previous-single-property-change (point) :ep-entry))
         (prev-to-prev (when prev (previous-single-property-change prev :ep-entry)))
         (prev-to-prev-to-prev (when prev-to-prev (previous-single-property-change prev-to-prev :ep-entry))))
    (cond
     ((and prev (ep-ep-entry-at-point prev) (not (eq (ep-ep-entry-at-point prev) (ep-ep-entry-at-point))))
      (goto-char prev)
      (point))
    ((and prev-to-prev (ep-ep-entry-at-point prev-to-prev))
     (goto-char prev-to-prev)
     (point))
    ((and prev-to-prev-to-prev (ep-ep-entry-at-point prev-to-prev-to-prev))
     (goto-char prev-to-prev-to-prev)
     (point))
    (t nil))))

(defun ep-next-entry-recenter (&optional n)
  "Move point to the beginning of the next entry. If the current
entry is the last one in the buffer, leave `point' unchanged. If
the new entry does not fit in the window, recenter point. With a
non-nil argument, skip N entries forwards."
  (interactive "p")
  (let ((n (or n 1)))
    (when (ep-ep-next-entry)
      (dotimes (dummy (- n 1)) (ep-ep-next-entry))
      (let* ((entry (ep-ep-entry-at-point))
             (boundaries (ep-ep-entry-boundaries entry))
             (start (car boundaries))
             (end (cdr boundaries)))
        (unless (and (pos-visible-in-window-p start) (pos-visible-in-window-p end))
          (let* ((start-line (line-number-at-pos start))
                 (end-line (line-number-at-pos end))
               (center-line (/ (window-height) 2)))
            (recenter (max 0 (- center-line (/ (- end-line start-line) 2))))))))))

(defun ep-previous-entry-recenter (&optional n)
  "Move point to the beginning of the previous entry. If the
current entry is the first one in the buffer, leave `point'
unchanged. If the new entry does not fit in the window, recenter
point. With a non-nil argument, skip N entries backwards."
  (interactive "p")
  (let ((n (or n 1)))
    (when (ep-ep-previous-entry)
      (dotimes (dummy (- n 1)) (ep-ep-previous-entry))
      (let* ((entry (ep-ep-entry-at-point))
             (boundaries (ep-ep-entry-boundaries entry))
             (start (car boundaries))
             (end (cdr boundaries)))
        (unless (and (pos-visible-in-window-p start) (pos-visible-in-window-p end))
          (let* ((start-line (line-number-at-pos start))
                 (end-line (line-number-at-pos end))
                 (center-line (/ (window-height) 2)))
            (recenter (max 0 (- center-line (/ (- end-line start-line) 2))))))))))

(defun ep-scroll-up ()
  (interactive)
  (scroll-up))

(defun ep-scroll-down ()
  (interactive)
  (scroll-down))


(defun ep-ep-section-entries-boundaries ()
  "Return the boundaries of the current section as a cons-pair (start . end)."
  (let* ((next (next-single-property-change (point) :ep-heading))
         (prev (previous-single-property-change (point) :ep-heading))
         (start (cond 
                 ((get-text-property (point) :ep-heading)
                  next)
                 (prev
                  prev)
                 ((get-text-property (- (point) 1) :ep-heading)
                  (point))
                 (t
                  (point-min))))
         (end (next-single-property-change start :ep-heading)))
    (unless end
      (setq end (point-max)))
    (cons start end)))

(defun ep-ep-sort-function (key)
  "Returns a function that sorts entries by KEY.
Any Preamble and String entries are sorted before any other entries."
  (lexical-let ((key key))
    (lambda (entries)
      (sort entries (lambda (entry-a entry-b)
                      (let ((type-a (ep-ep-alist-get-value "=type=" entry-a))
                            (type-b (ep-ep-alist-get-value "=type=" entry-b)))
                        (cond
                         ((string-equal type-a "Preamble") 
                          (if (string-equal type-b "Preamble") 
                              (string-lessp (ep-ep-alist-get-value "=content=" entry-a)
                                            (ep-ep-alist-get-value "=content=" entry-b))
                            t))

                         ((string-equal type-b "Preamble") nil)

                         ((string-equal type-a "String") 
                          (if (string-equal type-b "String")
                              (string-lessp (ep-ep-alist-get-value "=content=" entry-a)
                                            (ep-ep-alist-get-value "=content=" entry-b))
                            t))

                         ((string-equal type-b "String") nil)

                         (t
                          (string-lessp (ep-ep-alist-get-value key entry-a)
                                        (ep-ep-alist-get-value key entry-b))))))))))

(defun ep-sort-entries (&optional key interactive)
  "Sort the entries in the current buffer, ordering them by KEY."
  (interactive "i\np")
  (let ((key (or key 
                 (if interactive
                     (completing-read "Sort by [BibTeX key]: " ep-bib-fields)
                   "=key="))))

    (when (string-equal key "")
      (setq key "=key="))

    (ep-ep-redraw-entries (ep-ep-sort-function key))))

(defun ep-ep-redraw-entries (&optional func)
  "Redraw all entries. FUNC should be a function taking as a
single argument a list of entries, and returning a list of
entries to draw. If FUNC is nil, it defaults to `identity'."
  (let ((func (or func 'identity))
        (current-entry ep-ep-current-entry))
    (goto-char (point-min))
    (toggle-read-only -1)

    ;; Redraw each section separately
    (while (get-text-property (point) :ep-heading)
      (let* ((boundaries (ep-ep-section-entries-boundaries))
             (start (car boundaries))
             (end (cdr boundaries)))
        (narrow-to-region start end)
        (let ((entries (funcall func (ep-ep-extract-entries))))
          (delete-region start end)
          (ep-ep-format-entries entries))
        (widen)))

    (toggle-read-only 1)

    ;; Return to the previous entry
    (when (and current-entry (car (ep-ep-entry-boundaries current-entry)))
      (goto-char (car (ep-ep-entry-boundaries current-entry)))
      (ep-ep-highlight-entry))))

(defun ep-ep-filter-entries (entries filters)
  "Filter ENTRIES. FILTERS should be a list of
cons-cells (BibTeX-field . regexp), or a list of regexps."
  (let (res-entries match)
    (dolist (entry entries)
      (setq match t)
      (dolist (filter filters)
        (cond
         ((listp filter)
          (let ((field-val (ep-ep-alist-get-value (car filter) entry))
                (case-fold-search t))
            (unless (and field-val (string-match (cdr filter) field-val))
              (setq match nil))))
         ((stringp filter)
          (with-temp-buffer
            (ep-ep-format-entry entry)
            (goto-char (point-min))
            (unless (re-search-forward filter nil t)
              (setq match nil))))
         (t (error "Filter is neither list nor string: %S" filter))))
      (when match 
        (push entry res-entries)))
    (nreverse res-entries)))

(defun ep-regexp (regexp)
  "Create a new Emacs Paper buffer showing all entries matching
REGEXP."
  (interactive "sRegexp: ")

  (let* ((buffer-name (buffer-name))
         (entries (ep-ep-filter-entries (ep-ep-extract-entries) (list regexp))))
    (ep-ep-new-buffer (concat "Regexp-" (buffer-name))
      (ep-ep-insert-main-heading (concat "Entries in '" buffer-name "' matching '" regexp "'"))
      (ep-ep-format-entries entries))))

(defun ep-entries-with-tag (&optional tag)
  "Create a new Emacs Paper buffer showing all entries tagged
with TAG."
    (interactive "i")

    (let ((tag (or tag
                   (completing-read "Tag or regexp: " ep-ep-common-tags))))

      (let* ((buffer-name (buffer-name))
             (entries (ep-ep-filter-entries (ep-ep-extract-entries) 
                                            (list (cons "ep-tags" tag)))))
        (ep-ep-new-buffer (concat (buffer-name) ":" tag)
          (ep-ep-insert-main-heading (concat "Entries in '" buffer-name "' with tags matching '" tag "'"))
          (ep-ep-format-entries entries)))))

(defun ep-ep-goto-entry (entry)
  "Move POINT to start of ENTRY."
  (let ((boundaries (ep-ep-entry-boundaries entry)))
    (when boundaries
        (goto-char (car boundaries)))))

(defun ep-ep-entry-boundaries (entry)
  "Return the start and end point of ENTRY in a cons cell
as (START . END)."
  (save-excursion
    (let (found-entry)
      (goto-char (point-min))
      (if (eq entry (ep-ep-entry-at-point))
          (setq found-entry t))
      (while (and (not found-entry) (ep-ep-next-entry))
        (when (eq entry (ep-ep-entry-at-point))
          (setq found-entry t)))
      (if (not found-entry)
          nil
        (cons (point)
              (or (next-single-property-change (point) :ep-entry)
                  (point-max)))))))

;;; EP major mode

(defun ep-ep-highlight-entry ()
  "Highlight the entry at `point' and make it the current entry."
  (setq ep-ep-current-entry (ep-ep-entry-at-point))
  (let* ((boundaries (ep-ep-entry-boundaries ep-ep-current-entry))
         (start (car boundaries))
         (end (cdr boundaries)))
    (move-overlay ep-ep-highlight-overlay start end)))

(defun ep-ep-post-command-hook ()
  "Make sure that the entry at `point' is highlighted." 
  (let ((entry (ep-ep-entry-at-point)))
    (when (and entry (not (eq entry ep-ep-current-entry)))
      (ep-ep-highlight-entry))))

(define-derived-mode ep-ep-mode nil "Emacs Paper references"
  "Major mode for Emacs Paper reference listings.
\\\{ep-ep-mode-map}"

  (font-lock-mode -1)

  (make-variable-buffer-local 'ep-ep-current-entry)
  (set (make-variable-buffer-local 'ep-ep-undo-list) nil)
  (set (make-variable-buffer-local 'ep-ep-redo-list) nil)
  (make-variable-buffer-local 'ep-fix-titles)

  (set (make-variable-buffer-local 'ep-ep-highlight-overlay) (make-overlay (point) (point)))
  (overlay-put ep-ep-highlight-overlay 
               'face 
               (cons 'background-color ep-highlight-color))

  (add-hook (make-variable-buffer-local 'post-command-hook) 'ep-ep-post-command-hook nil t)

  (ep-ep-highlight-entry))

;; Key map for ep-ep-mode

(define-key ep-ep-mode-map "n" 'ep-next-entry-recenter)
(define-key ep-ep-mode-map "p" 'ep-previous-entry-recenter)
(define-key ep-ep-mode-map " " 'ep-scroll-up)
(define-key ep-ep-mode-map "\M-p" 'ep-scroll-down)

(define-key ep-ep-mode-map "m" 'ep-mark-entry)
(define-key ep-ep-mode-map "i" 'ep-import-entry)
(define-key ep-ep-mode-map "I" 'ep-import-marked-entries)

(define-key ep-ep-mode-map "e" 'ep-edit-entry)
(define-key ep-ep-mode-map "t" 'ep-add-tag)
(define-key ep-ep-mode-map "T" 'ep-remove-tag)

(define-key ep-ep-mode-map "\C-c\C-e" 'ep-new-entry)
(define-key ep-ep-mode-map "k" 'ep-kill-entry)

(define-key ep-ep-mode-map "w" 'ep-copy-entry)
(define-key ep-ep-mode-map "y" 'ep-yank-entry)

(define-key ep-ep-mode-map "o" 'ep-sort-entries)

(define-key ep-ep-mode-map "u" 'ep-inspire-update-entry)
(define-key ep-ep-mode-map "a" 'ep-arxiv-update-entry)
(define-key ep-ep-mode-map "f" 'ep-search)
(define-key ep-ep-mode-map "r" 'ep-regexp)

(define-key ep-ep-mode-map "b" 'ep-bib-find-file)
(define-key ep-ep-mode-map "s" 'ep-bib-save-file)
(define-key ep-ep-mode-map "q" 'ep-quit)

(define-key ep-ep-mode-map "g" 'ep-goto)
(define-key ep-ep-mode-map (kbd "RET") 'ep-goto-pdf)

(define-key ep-ep-mode-map (kbd "C-/") 'ep-undo)
(define-key ep-ep-mode-map (kbd "C-_") 'ep-undo)

(define-derived-mode  ep-ep-edit-mode bibtex-mode "EP BibTeX edit"
  "Major mode for editing Emacs Paper BibTeX entries.
\\\{ep-ep-edit-mode-map}")

;; Key map for ep-ep-edit-mode

(define-key ep-ep-edit-mode-map "\C-c\C-c" 'ep-ep-edit-done)
(define-key ep-ep-edit-mode-map "\C-g" 'ep-ep-edit-quit)

(defmacro ep-ep-new-buffer (name &rest body)
  "Create a new Emacs Paper buffer named NAME and execute BODY,
then go to the first entry and turn on Emacs Paper mode."
  (declare (indent defun))
  `(progn
     (let ((buf (generate-new-buffer ,name)))
       (switch-to-buffer buf)
       (buffer-disable-undo)
       (progn ,@body)
       (set-buffer-modified-p nil)
       (toggle-read-only 1)
       (goto-char (point-min))
       (ep-ep-next-entry)
       (ep-ep-mode)
       buf)))

(defun ep-ep-entry-at-point (&optional point)
  "Return the entry at POINT. If POINT is nil, use `point;"
  (let ((point (or point (point))))
    (get-text-property point :ep-entry)))

(defun ep-ep-extract-entries (&optional buffer)
  "Return the entries in BUFFER as a list. Default to
`current-buffer'"
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (goto-char (point-min))
    (let (entries)
      (while (ep-ep-next-entry)
        (push (ep-ep-entry-at-point) entries))
      (nreverse entries))))

(defun ep-ep-extract-marked-entries (&optional buffer)
  "Return all marked entries in BUFFER as a list. Default to
`current-buffer'"
  (if buffer
      (set-buffer buffer))
  (let ((overlays (overlays-in (point-min) (point-max)))
        entries)
    (dolist (overlay overlays)
      (when (overlay-get overlay :ep-mark)
        (push (ep-ep-entry-at-point (overlay-start overlay)) entries)))
    entries))

;; Entry editing

(defun ep-ep-update-entry (entry)
  "Redraw ENTRY."

  (let* ((highlight (eq entry ep-ep-current-entry))
         (boundaries (ep-ep-entry-boundaries entry))
         (start (car boundaries))
         (end (cdr boundaries))
         (overlays (overlays-at start))
         mark-overlay)
    (save-excursion
      (while (and overlays (not mark-overlay))
        (when (overlay-get (car overlays) :ep-mark)
          (setq mark-overlay (car overlays)))
        (pop overlays))
      (when mark-overlay
        (move-overlay mark-overlay start start))

      (toggle-read-only -1)
      (goto-char start)
      (delete-region (- start 1)
                     end)
      (ep-ep-format-entry entry)
      (toggle-read-only 1)
      (when mark-overlay
        (move-overlay mark-overlay start (point))))
    
    (when highlight
      (forward-char)
      (ep-ep-highlight-entry))))

(defun ep-ep-edit-done ()
  "Finish editing the entry."
  (interactive)
  (throw 'ep-edit-quit t))

(defun ep-ep-edit-quit ()
  "Quit editing the entry."
  (interactive)
  (throw 'ep-edit-quit nil))

(defun ep-ep-do-edit-entry (entry)
  "Edit ENTRY as a BibTeX entry. Return the updated entry or nil if the edit was cancelled."
  (let* ((edit-buffer (generate-new-buffer "EP edit entry"))
         new-entry)
    (switch-to-buffer edit-buffer)
    (buffer-disable-undo)
    (ep-bib-format-entry entry)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (ep-ep-edit-mode)
    (when (catch 'ep-edit-quit
            (recursive-edit))
      (setq new-entry (car (ep-bib-parse-buffer edit-buffer))))
    (kill-buffer edit-buffer)
    new-entry))

(defun ep-edit-entry (&optional entry)
  "Edit ENTRY as a BibTeX entry. Default to editing the current entry."
  (interactive)
  (let* ((entry  (or entry ep-ep-current-entry))
         (ep-buffer (current-buffer))
         (new-entry (ep-ep-do-edit-entry entry)))
    (switch-to-buffer ep-buffer)

    (if  (not new-entry)
        (message "Canceled")

      (unless (equal new-entry entry)
        (ep-ep-register-undo-edit-entry entry)
        (ep-ep-alist-clear entry)
        (dolist (field new-entry)
          (ep-ep-alist-set (car field) entry (cdr field)))
        (ep-ep-update-entry entry)))))

(defun ep-edit-tags (&optional entry)
  "Edit all tags of ENTRY using the minibuffer. Default to the
current entry."
  (interactive)
  (let* ((entry (or entry ep-ep-current-entry))
         (tags (ep-ep-field-value "ep-tags" entry))
         (new-tags (read-from-minibuffer "Tags: " tags)))
    (when (string-equal "" new-tags)
      (setq new-tags nil))
    (unless (equal new-tags tags)
      (ep-ep-register-undo-edit-entry entry)
      (ep-ep-alist-set "ep-tags" entry new-tags)
      (ep-ep-update-entry entry))))

(defun ep-add-tag (&optional tag entry)
  "Add TAG to ENTRY. If TAG is not given, prompt for it. Default
to the current entry."
  (interactive)
  (let* ((entry (or entry ep-ep-current-entry))
         (tags-val (ep-ep-field-value "ep-tags" entry))
         (tags (and tags-val (split-string tags-val ",")))
         (completion-ignore-case t)
         (tag (or tag (completing-read "Add tag: " ep-ep-common-tags nil nil)))
         new-tags)
    (if (member tag tags)
        (message "Entry already tagged with '%s'" tag)
      (setq new-tags (mapconcat 'identity (cons tag tags) ","))
      (when (string-equal "" new-tags)
        (setq new-tags nil))
      (ep-ep-register-undo-edit-entry entry)
      (ep-ep-alist-set "ep-tags" entry new-tags)
      (ep-ep-update-entry entry))))

(defun ep-remove-tag (&optional tag entry)
  "Remove TAG from ENTRY. If TAG is not given, prompt for
it. Default to the current entry."
  (interactive)
  (let* ((entry (or entry ep-ep-current-entry))
         (tags-val (ep-ep-field-value "ep-tags" entry))
         (tags (and tags-val (split-string tags-val ",")))
         tag new-tags)
    (if (not tags)
        (message "Entry has no tags")
      (setq tag (or tag (completing-read "Remove tag: " tags nil t)))
    (cond
     ((string-equal tag "")
      (message "Abort"))
     ((not (member tag tags))
        (message "Entry is not tagged with '%s' (How could this happen?)" tag))
     (t
      (setq new-tags (mapconcat 'identity (delete tag tags) ","))
      (when (string-equal "" new-tags)
        (setq new-tags nil))
      (ep-ep-register-undo-edit-entry entry)
      (ep-ep-alist-set "ep-tags" entry new-tags)
      (ep-ep-update-entry entry))))))


(defun ep-mark-entry (&optional entry mark)
  "Mark ENTRY if it is not marked, otherwise unmark it. Default
to the current entry. If MARK is 'mark, always mark ENTRY, if
MARK is 'unmark, unmark ENTRY."
  (interactive)
  (save-excursion
    (let* ((interactive (not entry))
           (entry (or entry ep-ep-current-entry))
           (boundaries (ep-ep-entry-boundaries entry))
           (start (car boundaries))
           (end (cdr boundaries))
           (overlays (overlays-at start))
           marked)
      (while (and overlays (not marked))
        (when (overlay-get (car overlays) :ep-mark)
          (setq marked (car overlays)))
        (pop overlays))
      (unless mark
        (if marked
            (setq mark 'unmark)
          (setq mark 'mark)))
      (cond 
       ((eq mark 'unmark)
        (when marked
          (delete-overlay marked)
          (when interactive
            (message "Entry unmarked")))
        nil)
       ((eq mark 'mark)
        (unless marked
          (let ((overlay (make-overlay start end)))
            (overlay-put overlay 'face 'bold)
            (overlay-put overlay 'before-string "* ")
            (overlay-put overlay :ep-mark t)
            (when interactive
              (message "Entry marked"))))
        t)))))

(defun ep-mark-entries (regexp unmark)
  "Mark all entries matching REGEXP. If UNMARK is non-nil, unmark the entries instead."
  (interactive "sRegexp: \nP")
  (let ((entries (ep-ep-filter-entries (ep-ep-extract-entries) (list regexp))))
    (dolist (entry entries)
      (ep-mark-entry entry (if unmark 'unmark 'mark)))
    (message "%d entries %s" (length entries) (if unmark "unmarked" "marked"))))

(defun ep-ep-insert-entry (entry buffer)
  (set-buffer buffer)
  (let ((entries (ep-ep-extract-entries))
        old-entry)
    (while (and entries (not old-entry))
      (when (or (eq entry (car entries))
                (and (ep-ep-alist-get-value "=key=" entry)
                     (equal (ep-ep-alist-get-value "=key=" entry) 
                            (ep-ep-alist-get-value "=key=" (car entries))))
                (and (ep-ep-alist-get-value "eprint" entry) 
                     (equal (ep-ep-alist-get-value "eprint" entry) 
                            (ep-ep-alist-get-value "eprint" (car entries)))))
        (setq old-entry entry))

      (pop entries))
    (cond 
     (old-entry
      nil)
     (t
      (goto-char (point-max))
      (toggle-read-only -1)
      (ep-ep-format-entry entry)
      (toggle-read-only 1)
      t))))

(defun ep-import-entry (&optional entry buffer)
  "Import ENTRY to BUFFER. Default to the current entry and the main Emacs Paper buffer."
  (interactive)
  
  (let ((entry (or entry ep-ep-current-entry))
        (buffer (or buffer ep-main-buffer)))
    (cond 
     ((not entry) (error "No entries to save"))
     ((not buffer) (error "Main buffer is not loaded"))
     (t
      (set-buffer buffer)
      (ep-ep-register-undo-insert-entry entry)
      (if (ep-ep-insert-entry entry buffer)
          (message "Entry saved to '%s'" (buffer-name buffer))
        (pop ep-ep-undo-list)
        (message "Entry already exists in '%s'" (buffer-name buffer)))))))

(defun ep-new-entry ()
  "Create a new entry in the current buffer and edit it."
  (interactive)
  (let ((entry '(("=type=" . "Article") ("author" . "") ("title" . ""))))
    (ep-import-entry (ep-ep-do-edit-entry entry) (current-buffer))))

(defun ep-yank-entry ()
  "Create a new entry from bibtex entry on top of kill-ring"
  (interactive)
  (let (new-entry)
    (with-temp-buffer
      (yank)
      (goto-char (point-min))
      (setq new-entry (car (ep-bib-parse-buffer (current-buffer)))))
    (if (not new-entry)
        (message "Not a BibTeX entry!")
      (ep-import-entry new-entry (current-buffer))
      (ep-ep-previous-entry))))

(defun ep-copy-entry ()
  "Copy the current entry."
  (interactive)
  (let ((entry ep-ep-current-entry))
    (with-temp-buffer
      (ep-bib-format-entry entry)
      (copy-region-as-kill (point-min) (point-max))
      (message "Entry copied to killed ring"))))

(defun ep-kill-entry ()
  "Kill the current entry."
  (interactive)
  (when ep-ep-current-entry)
    (ep-ep-register-undo-delete-entry ep-ep-current-entry)
    (ep-copy-entry)
    (ep-ep-delete-entry ep-ep-current-entry))

(defun ep-ep-delete-entry (entry)
  "Delete 'entry'."
  (let* ((boundaries (ep-ep-entry-boundaries entry))
         (entry-start (- (car boundaries) 1))
         (entry-end (cdr boundaries)))
    (toggle-read-only -1)
    (delete-region entry-start entry-end)
    (toggle-read-only 1)))
         
(defun ep-import-marked-entries ()
  "Import all marked entries to the main Emaca Paper buffer."
  (interactive)
  (let ((marked-entries (ep-ep-extract-marked-entries))
        (buffer ep-main-buffer)
        (saved 0))
    (if (not buffer)
        (error "Main Emacs paper buffer not loaded!")
      (set-buffer buffer)
      (dolist (entry marked-entries)
        (when (ep-ep-insert-entry entry buffer)
          (ep-ep-register-undo-insert-entry entry)
          (incf saved)))
      (message "Saved %d entries" saved))))

;;; Find entry online

(defun ep-goto (&optional arg)
  "Find this entry online. Query the user about how to look for the entry."
  (interactive "P")
  (let* ((collection '(("arXiv abstract" . 1) ("DOI" . 2) ("Spires record" . 3) ("PDF" . 4) ("Inspire record" . 5)))
         (completion-ignore-case t)
         (answer (completing-read "Go to [PDF]: " collection nil t nil nil "PDF")))
    (case (cdr (assoc answer collection))
      (1 (ep-goto-arxiv-abstract arg))
      (2 (ep-goto-doi arg))
      (3 (ep-goto-spires arg))
      (4 (ep-goto-pdf arg))
      (5 (ep-goto-inspire arg))
      (otherwise (error "Cannot go to '%s'" answer)))))

(defun ep-goto-arxiv-abstract (&optional arg)
  "Go to the arXiv abstract page of the current entry."
  (interactive "P")
  (let* ((entry ep-ep-current-entry)
         (url (ep-ep-concat-non-nil ep-arxiv-url "/abs/" (ep-ep-alist-get-value "eprint" entry))))
    (if url
        (browse-url url)
      (message "There is no preprint number for this entry. Trying at Inpire.")
      (ep-goto-inspire))))

(defun ep-goto-spires (&optional arg)
  "Go to the Spires record of the current entry."
  (interactive "P")
  (let* ((entry ep-ep-current-entry)
         (query (or (ep-ep-alist-get-value "=key=" entry)
                    (ep-ep-alist-get-value "eprint" entry)))
         (url (when query (ep-ep-spires-url (ep-ep-spires-guess-query query) "www"))))
    (if url
        (browse-url url)
      (message "There is no preprint number for this entry. Trying using DOI.")
      (ep-goto-doi))))

(defun ep-goto-inspire (&optional arg)
  "Go to the Inspire record of the current entry."
  (interactive "P")
  (let* ((entry ep-ep-current-entry)
         (query nil))
    (cond 
     ((ep-ep-alist-get-value "=key=" entry)
      (setq query (concat "find+texkey+" (ep-ep-alist-get-value "=key=" entry))))
     ((ep-ep-alist-get-value "eprint" entry)
      (setq query (concat "find+eprint+" 
                          (if (ep-ep-string-match-full "[0-9]\\{4\\}\\.[0-9]\\{4\\}" (ep-ep-alist-get-value "eprint" entry))
                              "arxiv:"
                            "")
                          (ep-ep-alist-get-value "eprint" entry)))))
    (if query
        (browse-url (concat ep-inspire-url query))
      (message "There is no preprint number for this entry. Trying using DOI.")
      (ep-goto-doi))))

 
(defun ep-goto-doi (&optional arg)
  "Follow the DOI of the current entry."
  (interactive "P")
  (let* ((entry ep-ep-current-entry)
         (url (ep-ep-concat-non-nil "http://dx.doi.org/" (ep-ep-alist-get-value "doi" entry))))
    (if url
        (browse-url url)
      (message "There is no DOI for the current entry."))))

;;; Connect to the arXiv 

(defun ep-ep-arxiv-parse-atom-buffer (buffer)
  "Parse the Atom file in BUFFER and return a list of entries."
  (let ((entry-list nil))
    (save-excursion
      (set-buffer buffer)
      
      (let* ((root (xml-parse-region (point-min) (point-max)))
             (feed (car root)))
        (dolist (entry (xml-get-children feed 'entry))
          (let* ((title-node (car (xml-get-children entry 'title)))
                 (summary-node (car (xml-get-children entry 'summary)))
                 (id-node (car (xml-get-children entry 'id)))
                 (category-node (car (xml-get-children entry 'arxiv:primary_category)))
                 (comment-node (car (xml-get-children entry 'arxiv:comment)))
                 (updated-node (car (xml-get-children entry 'updated)))
                 (published-node (car (xml-get-children entry 'published)))
                 (title (car (xml-node-children title-node)))
                 (summary (car (xml-node-children summary-node)))
                 (id (car (xml-node-children id-node)))
                 (category (xml-get-attribute category-node 'term))
                 (comment (car (xml-node-children comment-node)))
                 (published (car (xml-node-children published-node)))
                 (updated (car (xml-node-children updated-node)))
                 (author-list nil)
                 (author ""))

            (dolist (author-node (xml-get-children entry 'author))
              (setq author-list (append (cddar (xml-get-children author-node 'name))
                                        author-list)))
            (setq author-list (nreverse author-list))

            (while author-list
              (case (length author-list)
                (1 (setq author (concat author (car author-list))))
                (2 (setq author (concat author (car author-list) " and ")))
                (t (setq author (concat author (car author-list) " and " ))))
              (setq author-list (cdr author-list)))

            (if (string-equal (substring id 0 21) "http://arxiv.org/abs/")
                (setq id (substring id 21)))
            (if (string-equal (substring id -2 -1) "v")
                (setq id (substring id 0 -2)))

            (setq entry-list (append entry-list
                                     (list (list (cons "=type=" "Article")
                                                 (cons "eprint" id) 
                                                 (cons "author" author) 
                                                 (cons "title" title) 
                                                 (cons "abstract" summary) 
                                                 (cons "arxiv-comment" comment)
                                                 (cons "primaryClass" category)
                                                 (cons "=ep-updated=" (unless (string-equal published updated) updated))))))))))
    entry-list))

(defun ep-ep-arxiv-id-query (id-list)
  "Query the arxiv for the articles with identifiers in ID-LIST,
a list of strings. Returns a list of entries."
  (when id-list
    (let* ((id-string (mapconcat 'identity id-list ","))
           (query (concat "id_list=" id-string "&start=0&max_results=" (number-to-string (length id-list)))))
      (ep-ep-arxiv-api-query query))))

(defun ep-ep-arxiv-api-query (query)
  "Query the arxiv API using QUERY. Returns a list of entries."
  (save-excursion
    (let* ((url (concat ep-arxiv-api-url "?" query))
           (res-buf (ep-ep-url-retrieve-synchronously url))
           (xml-file (make-temp-file "ep-arxiv-" nil ".xml"))
           xml-file-buf
           entries)

      (set-buffer res-buf)
      (goto-char (point-min))
      (search-forward "<?xml")
      (beginning-of-line)

      ;; Write the XML data to a temporary file in order to
      ;; correctly interpret the utf-8 encoding (arXiv returns utf-8
      ;; encoded XML data but uses an iso-8859-1 HTTP header).
      (write-region (point) (point-max) xml-file)
      (kill-buffer res-buf)

      (setq xml-file-buf (find-file xml-file))
      ;; Don't clutter the file name history
      (ep-ep-pop-from-file-name-history xml-file)
      (setq entries (ep-ep-arxiv-parse-atom-buffer xml-file-buf))

      (kill-buffer xml-file-buf)
      (delete-file xml-file)

      entries)))

(defun ep-ep-arxiv-get-new-ids (category)
  "Retrive new entries from the arXiv for CATEGORY. Returns a
tripplet with three lists of article identifiers, corresponding
to new, cross-listed and updated articles."
  (let* ((res-buf (ep-ep-url-retrieve-synchronously (concat ep-arxiv-rss-url category)))
         (title-list (save-excursion
                       (set-buffer res-buf)
                       (let* ((root (xml-parse-region (point-min) (point-max)))
                              (rdf-node (car root))
                              (items (xml-get-children rdf-node 'item))
                              title-list)
                         (dolist (item items)
                           (let* ((title-node (car (xml-get-children item 'title)))
                                  (title (xml-node-children title-node)))
                             (setq title-list (append title-list title))))
                         title-list)))
         new-list cross-list updated-list)
    (kill-buffer res-buf)
    (with-temp-buffer
      (dolist (title title-list)
        (erase-buffer)
        (insert title)
        (search-backward-regexp "(arXiv:\\([^ ]*\\) \\(\\[[^]]*\\]\\)? ?\\([^)]*\\))" nil t)

        (cond ((string-equal (match-string 3) "UPDATED")
               (push (match-string 1) updated-list))
              ((not (string-equal (match-string 2) (concat "[" category "]")))
               (push  (match-string 1) cross-list))
              (t
               (push (match-string 1) new-list)))))

    (list (nreverse new-list)
          (nreverse cross-list) 
          (nreverse updated-list))))

(defun ep-arxiv-update-entry (&optional entry overwrite)
  "Update ENTRY by getting any missing fields from
arXiv."
  (interactive "i\nP")
  (let* ((entry (or entry ep-ep-current-entry))
         (eprint (ep-ep-alist-get-value "eprint" entry))
         (modified nil))
    (message (concat "Looking up " eprint " on the arXiv"))
    (let ((arxiv-entry (when eprint (car (ep-ep-arxiv-id-query (list eprint))))))
    (cond 
     ((not eprint) (message "%s" "The current entry has no preprint number"))
     (t
      (ep-ep-register-undo-edit-entry entry)
      (dolist (field arxiv-entry)
        (let ((field-name (car field))
              (field-val (cdr field)))
          (when (and (member field-name ep-bib-fields)
                     (or (and (not overwrite) 
                              (not (ep-ep-alist-get-value field-name entry)))
                         (and overwrite
                              (not (string-equal field-val (ep-ep-alist-get-value field-name entry))))))
            (setq modified t))
          (if (not overwrite)
              (ep-ep-alist-insert field-name entry field-val)
            (ep-ep-alist-set field-name entry field-val))))
      (with-silent-modifications
        (ep-ep-update-entry entry))
      (when modified
        (set-buffer-modified-p t))
      (message "Entry updated"))))))

(defun ep-check-arxiv (category) 
  "Show new entries at the arXiv for CATEGORY."
  (interactive
   (list (read-string (concat "arXiv category [" ep-arxiv-default-category "]: ") 
                      nil nil "hep-th")))
                            
  (let* ((ids (ep-ep-arxiv-get-new-ids category))
         (entries-new (ep-ep-arxiv-id-query (car ids)))
         (entries-cross-listed (ep-ep-arxiv-id-query (cadr ids)))
         (entries-updated (ep-ep-arxiv-id-query (caddr ids))))

    (if (not entries-new)
        (ep-ep-message "No new arXiv entries in %s" category)

      (dolist (entry entries-new)
        (ep-ep-fix-title entry))
      (dolist (entry entries-cross-listed)
        (ep-ep-fix-title entry)
        (ep-ep-alist-set "=ep-cross-list=" entry category))
      (dolist (entry entries-updated)
        (ep-ep-fix-title entry)
        (unless (string-equal (ep-ep-alist-get-value "primaryClass" entry) category)
          (ep-ep-alist-set "=ep-cross-list=" entry category)))

      (ep-ep-new-buffer (concat "EP arXiv: " category)
        (ep-ep-insert-main-heading (concat "New arXiv entries for category " category))
        (ep-ep-format-entries entries-new)

        (ep-ep-insert-sub-heading "Cross-listed entries")
        (ep-ep-format-entries entries-cross-listed)

        (ep-ep-insert-sub-heading  "Updated entries")
        (ep-ep-format-entries entries-updated)
        (ep-ep-message "Showing %d new entries, %d cross-listed entries and %d updated entries."
                 (length entries-new)
                 (length entries-cross-listed)
                 (length entries-updated))))))


(defun ep-arxiv-catch-up (category days)
  "Create an Emacs Paper buffer showing the entries in CATEGORY
from the last DAYS days."
  (interactive
   (list (read-string (concat "arXiv category [" ep-arxiv-default-category "]: ") nil nil "hep-th")
         (read-number "Number of days: ")))

  (message "Fetching entries from the last %d days on %s" days category)

  (let* ((end-date (date-to-time (format-time-string "%Y-%m-%d %Z")))
         (start-date (subtract-time end-date (days-to-time days)))
         (start (format-time-string "%Y%m%d" start-date))
         (end (format-time-string "%Y%m%d" end-date))
         (query (concat "search_query=cat:" category "+AND+lastUpdatedDate:%5b" start "+TO+" end "%5d&max_results=10000&sortBy=submittedDate&sortOrder=descending"))
         (entries (ep-ep-arxiv-api-query query)))

    (dolist (entry entries)
      (ep-ep-alist-set "abstract" entry nil)
      (ep-ep-fix-title entry)
      (unless (string-equal (ep-ep-alist-get-value "primaryClass" entry) category)
        (ep-ep-alist-set "=ep-cross-list=" entry category)))

    (ep-ep-new-buffer (concat "ArXiv " start " - " end )
      (ep-ep-insert-main-heading (concat "ArXiv entries in " category " " start " - " end ))
      (ep-ep-format-entries entries))))


;;; Inspire support

(defun ep-ep-inspire-guess-query (key)
  "Guess the Inspire query to find KEY."
  (concat

   (cond ((string-match "FIND " key)
          (replace-regexp-in-string " " "+" key))
         ((string-match " " key)
          (concat "FIND+" (replace-regexp-in-string " " "+" key)))
         ((ep-ep-string-match-full "[0-9]\\{4\\}\\.[0-9]\\{4\\}" key) 
          (concat "FIND+EPRINT+ARXIV:" key))        ; Match new arxiv identifier
         ((ep-ep-string-match-full "[0-9]\\{7\\}" key) 
          (concat "FIND+EPRINT+hep-th/" key)) ; Match old arxiv identifier
                                              ; (default to hep-th)
         ((ep-ep-string-match-full "[a-z\\-]+/[0-9]\\{7\\}" key) 
          (concat "FIND+EPRINT+" key))        ; Match old arxiv identifier
         ((ep-ep-string-match-full "[A-Za-z']*:[0-9]\\{4\\}[a-z]\\{2\\}[a-z]?" key) 
          (concat "FIND+TEXKEY+" key))        ; Match SPIRES key
         (t
          (concat "FIND+A+" key)))))          ; Default to author search

(defun ep-ep-inspire-extract-entries (query-buf)
  "Extract entries from a buffer resulting from a Inspires query
in QUERY-BUF. Return a list of entries. Kill QUERY-BUF after the
entries are extracted."
  (save-current-buffer
    (let (entries)
      (switch-to-buffer query-buf)
      
      (let* ((start (progn (goto-char (point-min))
                           (search-forward "<pre>" nil 't) 
                           (point)))
             (end (progn (goto-char (point-max))
                         (search-backward "</pre>" nil 't) 
                         (point))))
        (when (< start end)
          (narrow-to-region start end)
          (ep-ep-replace-regexp "<pre>" "")
          (ep-ep-replace-regexp "</pre>" "")
          (setq entries (ep-bib-parse-buffer query-buf))))
      (kill-buffer query-buf)

      (dolist (entry entries)
	(when ep-fix-titles
	  (ep-ep-fix-title entry))
	(ep-ep-fix-note entry))

      entries)))

(defun ep-ep-inspire-query-entries (query)
  "Perform a Inpire QUERY. Return a list of entries."
  (save-current-buffer
    (message (concat "Querying Inspire for '" query "'"))
    (let ((query-buf (ep-ep-url-retrieve-synchronously (ep-ep-inspire-url query))))
      (when query-buf
        (ep-ep-inspire-extract-entries query-buf)))))

(defun ep-ep-inspire-url (query &optional format)
  "Construct an url for a Inspire QUERY."
  (let ((format (or format "hx")))
    (concat ep-inspire-url
            query
            "&of=" format)))

(defun ep-inspire-update-entry (&optional entry overwrite)
  "Update ENTRY by getting any missing fields from Inspire. If
ENTRY is nil, default to the current entry. If OVERWRITE is
non-nil, replace any exisitng fields."
  (interactive "i\nP")
  (let* ((entry (or entry ep-ep-current-entry))
         (query (or (ep-ep-alist-get-value "=key=" entry)
                    (ep-ep-alist-get-value "eprint" entry)))
         (inspire-entry (when query 
                         (car (ep-ep-inspire-query-entries (ep-ep-inspire-guess-query query)))))
         (modified nil))
    (cond 
     ((not query) (message "%s" "The current entry has no key and no preprint number"))
     ((not inspire-entry) (message "%s" "The current entry was not found on Inspire"))
     (t
      (ep-ep-fix-title inspire-entry)
      (ep-ep-fix-note inspire-entry)
      (ep-ep-register-undo-edit-entry entry)
      (dolist (field inspire-entry)
        (let ((field-name (car field))
              (field-val (cdr field)))
          (when (and (member field-name ep-bib-fields)
                     (or (and (not overwrite) 
                              (not (ep-ep-alist-get-value field-name entry)))
                         (and overwrite
                              (not (string-equal field-val (ep-ep-alist-get-value field-name entry))))))
            (setq modified t))
          (if (not overwrite)
              (ep-ep-alist-insert field-name entry field-val)
            (ep-ep-alist-set field-name entry field-val))))
      (with-silent-modifications
        (ep-ep-update-entry entry))
      (when modified
        (set-buffer-modified-p t))
      (message "Entry updated")))))


;; Searching locally and in Inpire

(defun ep-search (query)
  "Search for QUERY in the main Emacs Paper buffer and in Inspire."
  (interactive "sSearch query: ")
  (let* ((inspire-query (ep-ep-inspire-guess-query query))
         (url (ep-ep-inspire-url inspire-query)))
    (ep-ep-new-buffer (concat "EP search results: " query)
       (ep-ep-insert-main-heading (concat "Search results for '" query "'"))
       (ep-ep-insert-sub-heading "Local results")
       (let* ((ep-query (ep-ep-search-parse-query inspire-query))
              (entries (ep-ep-filter-entries (ep-ep-extract-entries ep-main-buffer) ep-query)))
         (ep-ep-format-entries entries))
       (ep-ep-insert-sub-heading "Inpire results"))
    (message (concat "Looking up '" inspire-query "' at Inpire"))
    (url-retrieve url 'ep-ep-inspire-query-callback (list (current-buffer)))))

(defun ep-ep-search-parse-query (query)
  "Parse the Inpire formatted QUERY and returns a list of
cons-cells (BibTeX-field . regexp)."
  (let ((case-fold-search t)
        result)
    (setq query (replace-regexp-in-string "\\+" " " query))
    (setq query (replace-regexp-in-string "^FIND? " "" query))

    (dolist (elem (split-string query "\\( and \\| AND \\)"))
      (cond
       ((string-match "^a " elem)
        (push (cons "author" (substring elem 2)) result))
       ((string-match "^d " elem)
        (push (cons "year" (substring elem 2)) result))
       ((string-match "^t " elem)
        (push (cons "title" (substring elem 2)) result))
       ((string-match "^eprint arxiv:" elem)
        (push (cons "eprint" (substring elem 13)) result))
       ((string-match "^eprint " elem)
        (push (cons "eprint" (substring elem 7)) result))
       ((string-match "^texkey " elem)
        (push (cons "=key=" (substring elem 7)) result))
       (t
        (push elem result))))
    result))

(defun ep-ep-inspire-query-callback (status buf)
  "Insert entries returned by a Inpire query. Called by `url-retrieve' in `ep-search'."
  (let ((entries (ep-ep-inspire-extract-entries (current-buffer)))
        point)
    (message "Inserting matches from Inpire")
    (when (buffer-live-p buf)
      (switch-to-buffer buf)
      (setq point (point))
      (toggle-read-only -1)
      (goto-char (point-max))
      (ep-ep-format-entries entries)
      (toggle-read-only 1)
      (goto-char point))))

;;; Fix up titles and notes

(defun ep-ep-fix-title (entry)
  (when ep-fix-titles
    (with-temp-buffer
      (insert (ep-ep-alist-get-value "title" entry))
    
      (let* ((pre-replacements
             '(("\n" . " ")  ; Remove any newlines
               (" +" . " ")  ; Only single spaces
  
               ("^{" . "")   ; Remove start brace
               ("}$" . ""))) ; Remove end brace

            (replacements
             '(("AdS(5) *[x\\*] *S(5)" . "{$\\\\AdS_5 \\\\times \\\\Sphere^5$}")
               ("AdS(5) *[x\\*] *S\\*\\*5" . "{$\\\\AdS_5 \\\\times \\\\Sphere^5$}")
               ("AdS_?5 *[x\\*] *S^?5" . "{$\\\\AdS_5 \\\\times \\\\Sphere^5$}")
               ("AdS_?4 *[x\\*] *CP[_^]?3" . "{$\\\\AdS_4 \\\\times \\\\CP^3$}")
               ("AdS_?4 */ *CFT_?3" . "{$\\\\AdS_4/\\\\CFT_3$}")
               ("AdS(3) */ *CFT(2)" . "{$\\\\AdS_3/\\\\CFT_2$}")
               (" +N *= *4" . " {$\\\\superN = 4$}")
               (" +N *= *6" . " {$\\\\superN = 6$}")
               ("^N *= *4" . "{$\\\\superN = 4$}")
               ("^N *= *6" . "{$\\\\superN = 6$}")
               ("Yang" . "{Y}ang")
               ("Bethe" . "{B}ethe")
               ("Mill" . "{M}ill")
               ("Chern" . "{C}hern")
               ("Simons" . "{S}imons")
               ("Hirota" . "{H}irota")
               ("Baxter" . "{B}axter")
               ("Sitter" . "{S}itter")
               ("Wilson" . "{W}ilson")
               ("AdS */ *CFT" . "{A}d{S/CFT}")
               ("SU(2) *x *SU(2)" . "{$\\\\grSU(2) \\\\times \\\\grSU(2)$}")
               ("CP^3\\([^$]\\)" . "{$\\\\CP^3$}\\1")

               ("\\([^\\]\\)AdS" . "\\1{A}d{S}")

               ("^\\([A-Z0-9]+\\)-" . "{\\1}-")
               (" \\([A-Z0-9]+\\)-" . " {\\1}-")

               ("^\\([A-Z0-9]+\\) " . "{\\1} ")
               (" \\([A-Z0-9]+\\) " . " {\\1} ")
               (" \\([A-Z0-9]+\\)$" . " {\\1}")

               ("{\\([A-Z]\\)} " . "\\1 "))))

        (dolist (repl pre-replacements)
          (ep-ep-replace-regexp (car repl) (cdr repl)))

        (dolist (repl replacements)
          (ep-ep-replace-regexp (car repl) (cdr repl))))

      (ep-ep-alist-set "title" entry (buffer-substring (point-min) (point-max))))))

(defun ep-fix-title ()
  "Fix the title of the current entry"
  (interactive)
  (ep-ep-register-undo-edit-entry ep-ep-current-entry)
  (ep-ep-fix-title ep-ep-current-entry)
  (ep-ep-update-entry ep-ep-current-entry))

(defun ep-ep-fix-note (entry)
  "Rmove the \"* Temporary entry *\" note from Inspire entries."
  (when (and (ep-ep-alist-get-value "note" entry)
             (string-equal (ep-ep-alist-get-value "note" entry) "* Temporary entry *"))
    (ep-ep-alist-set "note" entry nil)))

;;  Main buffer

(defun ep-ep-main-start ()
  (when ep-pdf-file
    (ep-ep-pdf-read-file ep-pdf-file))
  (setq ep-main-buffer (ep-bib-load-file ep-main-bib-file)))

(defun ep-main ()
  "Load the main Emacs Paper buffer."
  (interactive)
  (cond 
   ((not (buffer-live-p ep-main-buffer)) (ep-ep-main-start))
   ((and (y-or-n-p "Emacs Paper main buffer is already open. Reread the main BibTeX file (this will kill the buffer)? ")
         (kill-buffer ep-main-buffer))
    (ep-ep-main-start))
   (t (switch-to-buffer ep-main-buffer))))
  
(defun ep-quit ()
  "Close the buffer."
  (interactive)

  (let ((buffer (current-buffer)))
    (when (kill-buffer buffer)
      (message "Closing Emacs Paper buffer")
      (when (eq buffer ep-main-buffer)
        (setq ep-main-buffer nil)))))


;; Handling PDF files

(defun ep-ep-url-retrieve-synchronously (url)
  "Work around `url-retrieve-synchronously` beeing really slow on my Mac."
  (let ((temp-file-name (concat ep-temp-dir (make-temp-name "ep-")))
        (buffer (generate-new-buffer (generate-new-buffer-name (concat " *" url "*")))))
    (when (ep-ep-url-retrieve-file url temp-file-name)
      (set-buffer buffer)
      (insert-file-contents temp-file-name))
    (delete-file temp-file-name)
    buffer))

(defun ep-ep-url-retrieve-file (url filename)
  (let* ((quote (if (equal system-type 'windows-nt) 
		    "\""
		  "'"))
	 (cmd (concat "curl " quote url quote " -L -s -S -f -m10 --create-dirs -o " quote filename quote))
        status)
    (setq status (shell-command cmd))
    (equal status 0)))

(defun ep-ep-pdf-read-file (filename)
  (let* ((xml-buffer (find-file-literally filename))
         (root (xml-parse-region (point-min) (point-max)))
         (papers (car root)))
    ;; Don't clutter the file name history
    (ep-ep-pop-from-file-name-history filename)

    (kill-buffer xml-buffer)
    
    (setq ep-pdf-list nil)

    (dolist (paper (xml-get-children papers 'paper))
      (let ((key (caddar (xml-get-children paper 'key)))
            (pdf (caddar (xml-get-children paper 'pdf))))
        (push (cons key pdf) ep-pdf-list)))))

(defun ep-ep-pdf-write-file (filename)
  (let ((xml-buffer (find-file-literally filename)))

    (erase-buffer)

    (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
    (insert "<papers>\n")
    (dolist (paper ep-pdf-list)
      (when (car paper)
        (insert "  <paper>\n")
        (insert "    <key>")
        (insert (car paper))
        (insert "</key>\n")
        (insert "    <pdf>")
        (insert (cdr paper))
        (insert "</pdf>\n")
        (insert "  </paper>\n")))
    (insert "</papers>\n")
    (save-buffer)

    ;; Don't clutter the file name history
    (ep-ep-pop-from-file-name-history filename)

    (kill-buffer xml-buffer)))

(defun ep-ep-open-pdf (filename)
  (shell-command (format ep-open-pdf-cmd (expand-file-name filename))))

(defun ep-goto-pdf (overwrite)
  (interactive "P")
  (let* ((entry ep-ep-current-entry)
         (key (ep-ep-alist-get-value "=key=" entry))
         (eprint (ep-ep-alist-get-value "eprint" entry))
         (pdf (ep-ep-alist-get-value key ep-pdf-list)))
    (if (and pdf (not overwrite))
        (ep-ep-open-pdf (concat ep-pdf-dir pdf))
      (message "Fetching %s" (or key ""))

      (if eprint
          (let* ((url (ep-ep-concat-non-nil ep-arxiv-url "/pdf/" eprint ".pdf"))
                 (pdfname (concat eprint ".pdf"))
                 (filename (concat ep-pdf-dir pdfname)))
            (if (not (and ep-pdf-file ep-pdf-dir (equal (current-buffer) ep-main-buffer)))
                  (browse-url url)
              (if (not (ep-ep-url-retrieve-file url filename))
                  (message "Failed to retrieve file from \"%s\"." url)
                (ep-ep-open-pdf filename)
                (ep-add-pdf filename))))
        (message "There is no preprint number for this entry. Trying using DOI. You need to manually save the PDF.")
        (ep-goto-doi)))))

(defun ep-add-pdf (filename)
  (interactive
   (list (read-file-name "PDF file: " ep-pdf-dir nil t)))
  (let* ((entry ep-ep-current-entry)
         (key (ep-ep-alist-get-value "=key=" entry))
         (pdfname (file-relative-name filename ep-pdf-dir)))
    (if (not key)
        (message "Cannot save a PDF for a paper without key.")
      (with-silent-modifications
        (ep-ep-alist-set key ep-pdf-list pdfname)
        (ep-ep-pdf-write-file ep-pdf-file)
        (when ep-add-pdf-to-itunes-automatically
          (ep-add-pdf-to-itunes))
        (ep-ep-update-entry entry)))))

(defun ep-add-pdf-to-itunes ()
  (interactive)

  (when ep-pdf-list
    (if (not ep-add-pdf-to-itunes-script)
        (message "Cannot find AppleScript to add entries to iTunes")
      (let* ((entry ep-ep-current-entry)
             (key (ep-ep-alist-get-value "=key=" entry))
             (title (ep-ep-alist-get-value "title" entry))
             (author (ep-ep-alist-get-value "author" entry))
             (year (ep-ep-alist-get-value "year" entry))
             (filename (ep-ep-alist-get-value key ep-pdf-list))
             (cmd (concat "osascript \"" ep-add-pdf-to-itunes-script "\" \"" ep-pdf-dir filename "\" \"" title "\" \"" author "\" " key " \"" year "\"" )))
        (if (not filename)
            (message "Entry %s does not have any associated PDF." key)
          (message "Running %s" cmd)
          (shell-command cmd))))))


;; Undo functionality

(defvar ep-ep-undo-list '() "Undo list")
(defvar ep-ep-redo-list '() "Redo list")

(defun ep-ep-undo-boundary ()
  (push nil ep-ep-undo-list)
  (push '(undo) ep-ep-undo-list))

(defun ep-ep-redo-boundary ()
  (push nil ep-ep-redo-list)
  (push '(redo) ep-ep-redo-list))

(defun ep-ep-register-undo (lst)
  (unless (eq last-command 'ep-undo)
    (setq ep-ep-undo-list (append ep-ep-redo-list ep-ep-undo-list))
    (setq ep-ep-redo-list nil))
  (ep-ep-undo-boundary)
  (push lst ep-ep-undo-list))

(defun ep-ep-register-redo (lst)
  (ep-ep-redo-boundary)
  (push lst ep-ep-redo-list))

(defun ep-ep-register-undo-edit-entry (entry)
  (ep-ep-register-undo (list 'apply 'ep-ep-undo-edit-entry entry (copy-alist entry))))

(defun ep-ep-register-redo-edit-entry (entry)
  (ep-ep-register-redo (list 'apply 'ep-ep-undo-edit-entry entry (copy-alist entry))))

(defun ep-ep-undo-edit-entry (args)
  (let ((entry (car args))
        (old-entry (cadr args)))
    (ep-ep-register-redo-edit-entry entry)
    (dolist (field old-entry)
      (ep-ep-alist-set (car field) entry (cdr field)))
    (ep-ep-update-entry entry)
    (ep-ep-goto-entry entry)))

(defun ep-ep-register-undo-insert-entry (entry)
  (ep-ep-register-undo (list 'apply 'ep-ep-undo-insert-entry entry)))

(defun ep-ep-register-redo-insert-entry (entry)
  (ep-ep-register-redo (list 'apply 'ep-ep-undo-insert-entry entry)))

(defun ep-ep-undo-insert-entry (args)
  (let ((entry (car args)))
    (ep-ep-goto-entry entry)
    (ep-ep-register-redo-delete-entry entry)
    (ep-ep-delete-entry entry)))

(defun ep-ep-register-undo-delete-entry (entry)
  (ep-ep-register-undo (list 'apply 'ep-ep-undo-delete-entry entry)))

(defun ep-ep-register-redo-delete-entry (entry)
  (ep-ep-register-redo (list 'apply 'ep-ep-undo-delete-entry entry)))

(defun ep-ep-undo-delete-entry (args)
  (let ((entry (car args)))
    (ep-ep-register-redo-insert-entry entry)
    (ep-ep-insert-entry entry (current-buffer))
    (ep-ep-goto-entry entry)))

(defun ep-undo ()
  "Undo last Emacs Paper action"
  (interactive)

  (unless (eq last-command 'ep-undo)
    (setq ep-ep-undo-list (append ep-ep-redo-list ep-ep-undo-list))
    (setq ep-ep-redo-list nil))

  (if (not ep-ep-undo-list)
      (message "No further undo information")
    (let (undo-item)
      (while (setq undo-item (pop ep-ep-undo-list))
        (let ((head (car undo-item)))
          (case head
            (apply
             (let ((func (cadr undo-item))
                   (args (cddr undo-item)))
               (funcall func args)))
            (undo
             (message "Undo!"))
            (redo
             (message "Redo!"))))))))

;; Extracting cited entries

(defun ep-extract-citations (&optional tex-file)
  "Extracts all BibTeX entries cited in 'tex-file' to a new Emacs Paper buffer"
  (interactive)

  ;; Ask user for the name of the TeX file and find aux file.
  (let* ((tex-file (or tex-file
                       (read-file-name (concat "TeX file (" (file-name-nondirectory (buffer-file-name)) "): "))))
         (aux-file (concat (file-name-sans-extension tex-file) ".aux"))
         (aux-buf (find-file aux-file))
         bib-file bib-buf keys entries all-entries)

    ;; Find citations
    (switch-to-buffer aux-buf)
    (goto-char (point-min))
    (while (re-search-forward "\\\\bibcite{\\([^}]*\\)}" nil t)
      (push (match-string 1) keys))

    ;; Find BibTeX file name
    (goto-char (point-min))
    (re-search-forward "\\\\bibdata{\\([^}]*\\)}" nil t)

    (setq bib-file (concat (match-string 1) ".bib"))
    (unless (file-name-absolute-p bib-file)
      (setq bib-file (concat (file-name-directory tex-file) bib-file)))

    (unless (file-exists-p bib-file)
      (setq bib-file (read-file-name (concat "File " bib-file " not found. "
                                             "BibTeX file to read entries from: "))))

    (kill-buffer aux-buf)

    (setq bib-buf (ep-bib-load-file bib-file))
    (setq all-entries (ep-ep-extract-entries bib-buf))
    (kill-buffer bib-buf)

    ;; Don't clutter the file name history
    (ep-ep-pop-from-file-name-history bib-file)
    (ep-ep-pop-from-file-name-history aux-file)

    ;; Extract cited entries, plus string entries and the preamble
    (dolist (entry all-entries)
      (when (or (string-equal (ep-ep-alist-get-value "=type=" entry) "Preamble") 
                (string-equal (ep-ep-alist-get-value "=type=" entry) "String")
                (member (ep-ep-alist-get-value "=key=" entry) keys))
        (push entry entries)))

    ;; Construct new EP buffer
    (ep-ep-new-buffer (concat "EP extracted entries")
      (ep-ep-insert-main-heading (concat "Emacs Paper -- Entries extracted from " (file-name-nondirectory bib-file) " cited in " (file-name-nondirectory tex-file)))
      (ep-ep-format-entries entries))
    (ep-sort-entries "=key=")
    (goto-char (point-min))))
