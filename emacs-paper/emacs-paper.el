;;; Set up variables
(defcustom ep-main-bib-file 
;;  "c:/Users/Olof/Documents/Jobb/Notes/References/refs.bib"
  "c:/Users/Olof/Documents/emacs-paper/test.bib"
  "Main BibTeX database used by Emacs Paper."
  :type 'string)

(defvar ep-entries nil
  "List of bibliography entries.")

(defvar ep-entries-changed nil
  "Set to t if Emacs Paper made any changes to the bibliography entries.")

(defvar ep-bib-fields
  '("author" "title" "journal" "volume" "number" "publisher" "year" "month" 
    "edition" "address" "pages" "eprint" "archivePrefix" "primaryClass"
    "doi" "printed" "SLACcitation" "note")
  "BibTeX fields saved by Emacs Paper. The fields are inserted in
  the order of appearance in the list."  )


;;; Set up and tear down 

(defun ep-start ()
  "Start Emacs Paper.

Read in the main BibTeX database."
  (interactive)

  (when (vc-backend ep-main-bib-file)
    ;; TODO: make sure file is up to date...
    )

  (setq ep-entries (ep-parse-bib-file ep-main-bib-file))
)

(defun ep-stop ()
  "Stops Emacs Paper.

Write out the main BibTeX database."
  (interactive)

  ;; TODO: only save if anything has changed...

  (if (not ep-entries)
      (message "%s" 
               (concat "No entries found in `ep-entries'. "
                       "Either Emacs Paper is not started or `ep-main-bib-file' contains no entries. "
                       "The file will not be overwritten."))
    (ep-save-bib-entries))

  (when (vc-backend ep-main-bib-file)
    ;; TODO: make sure file is comitted
    )

  ;; Unset variables
  (setq ep-entries nil)
  (setq ep-entries-changed nil)
)


;;; General helper functions 

(defun ep-cleanup-whitespace (string)
  "Remove any tabs, newlines and double spaces from STRING."
  (while (string-match "[\n\t]+" string)
    (setq string (replace-match " " nil t string)))
  (while (string-match "  +" string)
    (setq string (replace-match " " nil t string)))
  string)


;;; Entry helper functions

(defun ep-field-value (field-name entry)
  "Return the value of FIELD in ENTRY if set, or nil."
  (cdr (assoc-string field-name entry)))


;;; Loading and saving BibTeX files

(defun ep-parse-bib-file (file)
  "Find FILE and parse all BibTeX entries. Return a list of the
parsed entries."
  (let ((orig-buf (current-buffer)))
    (find-file-read-only file)
    (let ((progress (make-progress-reporter "Parsing entries..." (point-min) (point-max)))
          entries)
      (save-excursion
        (goto-char (point-min))
        
        (while (search-forward "@" nil t)
          (backward-char)
          (let* ((entry (bibtex-parse-entry t)))
            (dolist (field entry)
              (setcdr field (ep-cleanup-whitespace (cdr field))))
            (push entry entries))
          (progress-reporter-update progress (point)))
        (switch-to-buffer orig-buf)
        (progress-reporter-done progress)
        (nreverse entries)))))

(defun ep-save-bib-entries ()
  (interactive)
  "Save the BibTeX entries in `ep-entries' to `ep-main-bib-file'"
  (let ((orig-buf (current-buffer)))
    (find-file ep-main-bib-file)
    (toggle-read-only -1)
    (ep-write-bib-entries ep-entries)
    (save-buffer)
    (toggle-read-only +1)
    (switch-to-buffer orig-buf)))

(defun ep-write-bib-entries (entries)
  "Write ENTRIES as BibTeX entries to current buffer.

Warning: the buffer will be erased before the entries are
inserted!

The file is not saved."
  (let ((did-write nil))
    (widen)
    (unless (and (buffer-modified-p)
                 (not (y-or-n-p (concat "File " (buffer-name) " has been changed."
                                        " Inserting entries will overwrite these changes."
                                        " Do you want to continue?")))
                 )
      (erase-buffer)
      (insert "This file was ceated by Emacs Paper.\n\n")
      (ep-bib-insert-entries entries)
      (insert "%% Local Variables:\n%% mode: fundamental\n%% End:\n")
      (setq did-write t))
    did-write))

(defun ep-bib-insert-entries (entries)
  "Insert ENTRIES into current buffer."
  (let ((progress (make-progress-reporter "Insering entries..." 0 (length entries)))
        (counter 0))
    (dolist (entry entries)
      (ep-bib-insert-entry entry)
      (setq counter (+ 1 counter))
      (progress-reporter-update progress counter)
      (insert "\n\n"))
    (progress-reporter-done progress)))


(defun ep-bib-insert-entry (entry)
  "Insert ENTRY in current buffer."
  (insert "@" (ep-field-value "=type=" entry) (bibtex-entry-left-delimiter))
  (insert (ep-field-value "=key=" entry))
  (dolist (field-name ep-bib-fields)
    (let ((field-value (ep-field-value field-name entry)))
      (if field-value
          (bibtex-make-field (list field-name nil field-value nil)))))
  (insert "\n")
  (insert (bibtex-entry-right-delimiter)))


;;; Inserting, finding and merging entries

(defun ep-find-entry (entry-in entries-in &optional field-name-in)
  (let* ((field-name (if field-name-in 
                         field-name-in
                       "=key="))
         (key (ep-field-value field-name entry-in))
         (entries entries-in)
        found-entry)
    (while (and entries (not found-entry))
      (let ((entry (pop entries)))
        (when (string-equal key (ep-field-value field-name entry))
          (setq found-entry entry))))
    found-entry))

(defun ep-merge-entry (entry-a entry-b)
  "Add in place any undefined fields from ENTRY-B to
ENTRY-A. Return the merged entry."
  (dolist (field entry-b)
    (unless (assoc (car field) entry-a)
      (setcdr (last entry-a) (list field))))
  entry-a)

(defun ep-insert-entry (entry-in entries-in &optional field-name-in)
  "If an entry with an equal field value for FIELD-NAME-IN exists
in ENTRIES-IN, ENTRY-IN is merged to it. Otherwise, ENTRY-IN will
is added to the end of ENTRIES-IN.

The defauly value of FIELD-NAME-IN is \"=key=\"."
  (unless entries-in
    (error "FIELD-NAME-IN should not be empty"))
  (let ((entry (ep-find-entry entry-in entries-in field-name-in)))
    (if entry
        (ep-merge-entry entry entry-in)
      (setcdr (last entries-in) (list entry-in))))
  entries-in)

;;; Main EP interface

;; Some formating helper macros

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


(defun ep-ep-format-entry (entry)
  "Insert ENTRY nicely fromatted into current buffer."
  (or (ep-ep-insert-non-nil "(" (ep-field-value "=key=" entry) ")\n")
      (ep-ep-insert-non-nil (ep-field-value "eprint" entry)  " "
                            "[" (ep-field-value "primaryClass" entry) "]" "\n")
      (ep-ep-insert-non-nil " " (ep-field-value "eprint" entry)  "\n"))

  (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "title" entry) 'face '(:weight bold :slant italic :height 1.2)) "\n")
  (ep-ep-insert-non-nil (ep-field-value "author" entry) "\n")

  (if (string-equal "JHEP" (ep-field-value "journal" entry))
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "journal" entry) 'face 'italic ) " " 
                            (ep-ep-propertize-non-nil 
                             (ep-ep-concat-non-nil (ep-ep-substring-non-nil (ep-field-value "year" entry) 2)
                                                   (ep-field-value "volume" entry))
                             'face 'bold) ", "
                            (ep-field-value "pages" entry) " "
                            "(" (ep-field-value "year" entry) ")")
    (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "journal" entry) 'face 'italic ) " "
                          (ep-ep-propertize-non-nil (ep-field-value "volume" entry) 'face 'bold) ", "
                          (ep-field-value "pages" entry) " "
                          "(" (ep-field-value "year" entry) ")"))

  (when (ep-field-value "=key=" entry)
    (when (and (ep-field-value "journal" entry) (ep-field-value "eprint" entry))
      (insert ", "))
    (or (ep-ep-insert-non-nil (ep-field-value "eprint" entry)  " "
                              "[" (ep-field-value "primaryClass" entry) "]")
        (ep-ep-insert-non-nil " " (ep-field-value "eprint" entry))))
  (insert ".\n"))

(defun ep-ep-insert-entries (entries heading)
  (insert (propertize heading 'face '(:weight bold :height 1.5 :underline t)))
  (insert "\n\n")

  (dolist (entry ep-entries)
    (let ((start-point (point)))
       (ep-ep-format-entry entry)
       (put-text-property start-point (point) :ep-entry entry)
       (insert "\n")
       (dotimes (n 72)
           (insert (propertize " " 'face '(:strike-through t))))
       (insert "\n\n"))))


(defun ep-ep-next-entry ()
  (interactive)
  (let* ((next (next-single-property-change (point) :ep-entry))
         (next-to-next (next-single-property-change next :ep-entry)))
    (cond
     ((and next (get-text-property next :ep-entry))
           (goto-char next))
     ((and next-to-next (get-text-property next-to-next :ep-entry))
           (goto-char next-to-next)))))

(defun ep-ep-previous-entry ()
  (interactive)
  (let* ((prev (previous-single-property-change (point) :ep-entry))
         (prev-to-prev (previous-single-property-change prev :ep-entry)))
    (cond
     ((and prev (get-text-property prev :ep-entry))
           (goto-char prev))
     ((and prev-to-prev (get-text-property prev-to-prev :ep-entry))
           (goto-char prev-to-prev)))))

(progn
  (switch-to-buffer "ep-test")
  (font-lock-mode -1)
  (ep-ep-insert-entries ep-entries "Emacs Paper references")
  (goto-char (point-min))
  (ep-ep-next-entry))

;;; Connect to the ArXiv 


;;; Connect to Spires

