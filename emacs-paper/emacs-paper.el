;;; Set up variables
(defcustom ep-main-bib-file 
;;  "c:/Users/Olof/Documents/Jobb/Notes/References/refs.bib"
  "c:/Users/Olof/Documents/emacs-paper/test.bib"
  "Main BibTeX database used by Emacs Paper."
  :type 'string)

(defvar ep-entries nil
  "List of bibliography entries.")

(defvar ep-bib-fields
  '("author" "title" "journal" "volume" "number" "publisher" "year" "month" 
    "edition" "address" "pages" "eprint" "archivePrefix" "primaryClass"
    "doi" "printed" "SLACcitation" "note")
  "BibTeX fields saved by Emacs Paper. The fields are inserted in
  the order of appearance in the list."  )

(defvar ep-ep-highlight-overlay nil
  "Overlay used to highlight the current entry.")

(defvar ep-ep-current-entry nil
  "The current active entry in the buffer.")

(defcustom ep-arxiv-default-category "hep-th"
  "Default arXiv category to use when checking for new articles."
  :type 'string)

;;; Set up and tear down 

(defun ep-start ()
  "Start Emacs Paper.

Read in the main BibTeX database."
  (interactive)

  (when (vc-backend ep-main-bib-file)
    ;; TODO: make sure file is up to date...
    )

  (setq ep-entries (ep-bib-parse-file ep-main-bib-file))

  (ep-ep-main))

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
    (ep-bib-save-entries))

  (when (vc-backend ep-main-bib-file)
    ;; TODO: make sure file is comitted
    )

  ;; Unset variables
  (setq ep-entries nil))


;;; General helper functions 

(defun ep-cleanup-whitespace (string)
  "Remove any tabs, newlines and double spaces from STRING."
  (while (string-match "[\n\t]+" string)
    (setq string (replace-match " " nil t string)))
  (while (string-match "  +" string)
    (setq string (replace-match " " nil t string)))
  string)

(defun ep-string-match-full (regexp string)
  "Check if 'string' exactly matches 'regexp'"
  (and (string-match regexp string)
	   (equal (match-beginning 0) 0)
	   (equal (match-end 0) (length string))))

(defun ep-alist-insert (key alist val)
  "Insert (KEY . VAL) into alist, unless there already is an
entry for KEY."
  (unless (ep-alist-get-value key alist)
    (if (not (assoc key alist))
        (setcdr (last alist) (list (cons key val)))
      (setcdr (assoc key alist) val)
      (list (cons key val)))))


(defun ep-alist-set (key alist val)
  "Set the value of KEY in ALIST to VAL. Add the entry if it does
not exist."
  (let ((field (assoc key alist)))
    (if field
        (setcdr field val)
      (ep-alist-insert key alist val))))

(defun ep-alist-get-value (key alist)
  "Get the value of KEY in ALIST."
  (cdr (assoc key alist)))

(defun ep-alist-clear (alist)
  "Set all values in ALIST to nil."
  (dolist (field alist)
    (setcdr field nil)))



;;; Entry helper functions

(defun ep-field-value (field-name entry)
  "Return the value of FIELD in ENTRY if set, or nil."
  (cdr (assoc-string field-name entry)))


;;; Loading and saving BibTeX files

(defun ep-bib-parse-file (file)
  "Find FILE and parse all BibTeX entries. Return a list of the
parsed entries."
  (let* ((orig-buf (current-buffer))
         (bib-buf (find-file-read-only file))
         (entries (ep-bib-parse-buffer bib-buf)))
    (switch-to-buffer orig-buf)
    entries))

(defun ep-bib-parse-buffer (buffer)
  "Parse all BibTeX entries in BUFFER. Return a list of the
parsed entries."
  (let ((orig-buf (current-buffer)))
    (set-buffer buffer)
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


(defun ep-bib-save-entries ()
  (interactive)
  "Save the BibTeX entries in `ep-entries' to `ep-main-bib-file'"
  (let ((orig-buf (current-buffer)))
    (find-file ep-main-bib-file)
    (toggle-read-only -1)
    (ep-bib-write-entries ep-entries)
    (save-buffer)
    (toggle-read-only +1)
    (switch-to-buffer orig-buf)))

(defun ep-bib-write-entries (entries)
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
;; OBS: not tested!
    (ep-alist-insert (car field) entry-a (cdr field)))
;;    (unless (assoc (car field) entry-a)
;;      (setcdr (last entry-a) (list field))))
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

;;; EP buffer formatting

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


(defun ep-ep-insert-entry (entry)
  "Insert ENTRY nicely fromatted into current buffer."
  (let* ((start-point (point)))
    (or (ep-ep-insert-non-nil (ep-field-value "=key=" entry) "\n")
        (ep-ep-insert-non-nil (ep-field-value "eprint" entry)  " "
                              "[" (ep-field-value "primaryClass" entry) "]" "\n")
        (ep-ep-insert-non-nil " " (ep-field-value "eprint" entry)  "\n"))
    
    (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "title" entry) 'face '(:weight bold :slant italic :height 1.1)) "\n")
    (ep-ep-insert-non-nil (ep-field-value "author" entry))

    (when (or (ep-field-value "year" entry) 
              (ep-field-value "journal" entry)
              (and (ep-field-value "=key=" entry) (ep-field-value "eprint" entry)))
      (insert "\n"))
    
    (if (string-equal "JHEP" (ep-field-value "journal" entry))
        (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "journal" entry) 'face 'italic ) " " 
                              (ep-ep-propertize-non-nil 
                               (ep-ep-concat-non-nil (ep-ep-substring-non-nil (ep-field-value "year" entry) 2)
                                                     (ep-field-value "volume" entry))
                               'face 'bold) ", "
                               (ep-field-value "pages" entry) " ")
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "journal" entry) 'face 'italic ) " "
                            (ep-ep-propertize-non-nil (ep-field-value "volume" entry) 'face 'bold) ", "
                            (ep-field-value "pages" entry) " "))
    (ep-ep-insert-non-nil "(" (ep-field-value "year" entry) ")")
    
    (when (ep-field-value "=key=" entry)
      (when (and (or (ep-field-value "journal" entry)
                     (ep-field-value "year" entry))
                 (ep-field-value "eprint" entry))
            (insert ", "))
      (or (ep-ep-insert-non-nil (ep-field-value "eprint" entry)  " "
                                "[" (ep-field-value "primaryClass" entry) "]")
          (ep-ep-insert-non-nil (ep-field-value "eprint" entry))))
    (insert ".\n")

    (when (ep-field-value "abstract" entry)
      (insert "\n")
      (insert (ep-field-value "abstract" entry)))

    (put-text-property start-point (point) :ep-entry entry)))

(defun ep-ep-insert-main-heading (heading)
  "Insert HEADING in current buffer."
  (toggle-read-only -1)
  (unless (bobp)
    (insert "\n"))
  (insert (propertize heading 'face '(:weight bold :height 1.5 :underline t)))
  (insert "\n\n")
  (toggle-read-only 1))

(defun ep-ep-insert-sub-heading (heading)
  "Insert HEADING in current buffer."
  (toggle-read-only -1)
  (unless (bobp)
    (insert "\n"))
  (insert (propertize heading 'face '(:weight bold :height 1.2 :underline t)))
  (insert "\n\n")
  (toggle-read-only 1))

(defun ep-ep-insert-entries (entries)
  "Insert ENTRIES in current buffer."
  (toggle-read-only -1)

  (dolist (entry entries)
    (ep-ep-insert-entry entry)
    (insert "\n"))
  (toggle-read-only 1))


(defun ep-ep-highlight-entry ()
  (setq ep-ep-current-entry (get-text-property (point) :ep-entry))
  (let* ((boundaries (ep-ep-entry-boundaries ep-ep-current-entry))
         (start (car boundaries))
         (end (cdr boundaries)))
    (move-overlay ep-ep-highlight-overlay start end)))


;;; EP buffer navigation

(defun ep-ep-next-entry ()
  "Move point to the beginning of the next entry and return
`point'. If the current entry is the last one in the buffer,
leave `point' unchanged and return nil."
  (let* ((next (next-single-property-change (point) :ep-entry))
         (next-to-next (when next (next-single-property-change next :ep-entry))))
    (cond
     ((and next (get-text-property next :ep-entry))
      (goto-char next) 
      (point))
     ((and next-to-next (get-text-property next-to-next :ep-entry))
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
     ((and prev-to-prev (get-text-property prev-to-prev :ep-entry))
           (goto-char prev-to-prev)
           (point))
     ((and prev-to-prev-to-prev (get-text-property prev-to-prev-to-prev :ep-entry))
           (goto-char prev-to-prev-to-prev)
           (point))
     (t nil))))

(defun ep-ep-next-entry-recenter ()
  "Move point to the beginning of the next entry. If the current
entry is the last one in the buffer, leave `point' unchanged. If
the new entry does not fit in the window, recenter point."
  (interactive)
  (when (ep-ep-next-entry)
    (let* ((entry (get-text-property (point) :ep-entry))
           (boundaries (ep-ep-entry-boundaries entry))
           (start (car boundaries))
           (end (cdr boundaries)))
      (unless (and (pos-visible-in-window-p start) (pos-visible-in-window-p end))
        (let* ((start-line (line-number-at-pos start))
               (end-line (line-number-at-pos end))
               (center-line (/ (window-height) 2)))
          (recenter (max 0 (- center-line (/ (- end-line start-line) 2)))))))))

(defun ep-ep-previous-entry-recenter ()
  "Move point to the beginning of the previous entry. If the
current entry is the first one in the buffer, leave `point'
unchanged. If the new entry does not fit in the window, recenter
point."
  (interactive)
  (when (ep-ep-previous-entry)
    (let* ((entry (get-text-property (point) :ep-entry))
           (boundaries (ep-ep-entry-boundaries entry))
           (start (car boundaries))
           (end (cdr boundaries)))
      (unless (and (pos-visible-in-window-p start) (pos-visible-in-window-p end))
        (let* ((start-line (line-number-at-pos start))
               (end-line (line-number-at-pos end))
               (center-line (/ (window-height) 2)))
          (recenter (max 0 (- center-line (/ (- end-line start-line) 2)))))))))

(defun ep-ep-entry-boundaries (entry)
  (save-excursion
    (let (found-entry)
      (goto-char (point-min))
      (while (and (not found-entry) (ep-ep-next-entry))
        (when (eq entry (get-text-property (point) :ep-entry))
          (setq found-entry t)))
      (if (not found-entry)
          nil
        (cons (point) (next-single-property-change (point) :ep-entry))))))

;;; EP major mode

(defun ep-ep-post-command-hook ()
  (when ep-ep-current-entry
    (let ((current-entry (get-text-property (point) :ep-entry)))
      (when (and current-entry (not (eq current-entry ep-ep-current-entry)))
        (ep-ep-highlight-entry)))))

(define-derived-mode ep-ep-mode nil "Emacs Paper references"
  "Major mode for Emacs Paper reference listings.
\\\{ep-ep-mode-map}"

  (font-lock-mode -1)

  (make-variable-buffer-local 'ep-ep-current-entry)

  (set (make-variable-buffer-local 'ep-ep-highlight-overlay) (make-overlay (point) (point)))
  (overlay-put ep-ep-highlight-overlay 'face '(background-color . "honeydew"))

  (add-hook (make-variable-buffer-local 'post-command-hook) 'ep-ep-post-command-hook nil t)

  (ep-ep-highlight-entry))

(define-key ep-ep-mode-map "n" 'ep-ep-next-entry-recenter)
(define-key ep-ep-mode-map "p" 'ep-ep-previous-entry-recenter)

(defun ep-ep-main ()
  "Set up main Emacs Paper reference buffer."
  (interactive)
  (switch-to-buffer "EP-main")
  (toggle-read-only -1)
  (erase-buffer)
  (ep-ep-insert-main-heading "Emacs Paper references")
  (ep-ep-insert-entries ep-entries)
  (goto-char (point-min))
  (ep-ep-next-entry)
  (ep-ep-mode))

(define-derived-mode  ep-ep-edit-mode bibtex-mode "EP BibTeX edit"
  "Major mode for editing Emacs Paper BibTeX entries.
\\\{ep-ep-edit-mode-map}")

(define-key ep-ep-edit-mode-map "\C-c\C-c" 'ep-ep-edit-done)

(defun ep-ep-update-entry (entry &optional new-entry)
  "Redraw ENTRY. If NEW-ENTRY is non-nil, first replace ENTRY by
NEW-ENTRY."
  (when new-entry
    (ep-alist-clear entry)
    (dolist (field new-entry)
      (ep-alist-set (car field) entry (cdr field))))
  
  (let* ((boundaries (ep-ep-entry-boundaries entry))
         (start (car boundaries))
         (end (cdr boundaries)))
    (toggle-read-only -1)
    (goto-char start)
    (delete-region start end)
    (ep-ep-insert-entry entry)
    (goto-char start)
    (toggle-read-only 1)))

(defun ep-ep-edit-done ()
  "Finish editing the entry."
  (interactive)
  (throw 'ep-edit-quit t))

(defun ep-ep-edit-current-entry ()
  (interactive)
  (let ((new-entry (ep-ep-edit-entry ep-ep-current-entry)))
    (ep-ep-update-entry ep-ep-current-entry new-entry)
    (ep-ep-highlight-entry)))

(defun ep-ep-edit-entry (entry)
  "Edit ENTRY as a BibTeX entry. Return the new entry."
  (let ((orig-buffer (current-buffer))
        (edit-buffer (generate-new-buffer "EP edit entry"))
        new-entry)
    (switch-to-buffer edit-buffer)
    (ep-bib-insert-entry entry)
    (goto-char 0)
    (ep-ep-edit-mode)
    (catch 'ep-edit-quit
      (recursive-edit))
    (setq new-entry (car (ep-bib-parse-buffer edit-buffer)))
    (kill-buffer edit-buffer)
    (switch-to-buffer orig-buffer)

    new-entry))

;;; Connect to the arXiv 

(defun ep-arxiv-parse-atom-buffer (buffer)
  (let ((entry-list nil))
    (save-excursion
      (set-buffer buffer)
      
      (let* ((root (xml-parse-region (point-min) (point-max)))
             (feed (car root)))
        (dolist (entry (xml-get-children feed 'entry))
          (let* ((title-node (car (xml-get-children entry 'title)))
                 (summary-node (car (xml-get-children entry 'summary)))
                 (id-node (car (xml-get-children entry 'id)))
                 (category-node (car (xml-get-children entry 'category)))
                 (title (car (xml-node-children title-node)))
                 (summary (car (xml-node-children summary-node)))
                 (id (car (xml-node-children id-node)))
                 (category (xml-get-attribute category-node 'term))
                 (author-list nil)
                 (author ""))
            (dolist (author-node (xml-get-children entry 'author))
              (setq author-list (append (cddar (xml-get-children author-node 'name))
                                        author-list)))

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
                                     (list (list (cons "eprint" id) 
                                                 (cons "author" author) 
                                                 (cons "title" title) 
                                                 (cons "abstract" summary) 
                                                 (cons "primaryClass" category)))))))))
    entry-list))

(defun ep-arxiv-id-query (id-list)
  "Query the arxiv for the articles with identifiers in ID-LIST,
a list of strings. Returns a list of entries."
  (when id-list
    (save-excursion
      (let* ((id-string (mapconcat 'identity id-list ","))
             (url (concat "http://export.arxiv.org/api/query?id_list="
                          id-string "&start=0&max_results=" 
                          (number-to-string (length id-list))))
             (res-buf (url-retrieve-synchronously url))
             entries)
        (set-buffer res-buf)
        (goto-char (point-min))
        (search-forward "<?xml")
        (beginning-of-line)
        (delete-region (point-min) (point))
        (setq entries (ep-arxiv-parse-atom-buffer res-buf))
        (kill-buffer res-buf)
        entries))))

(defun ep-arxiv-get-new-ids (category)
  "Retrive new entries from the arXiv for CATEGORY. Returns a
tripplet with three lists of article identifiers, corresponding
to new, cross listed and updated articles."
  (let* ((res-buf (url-retrieve-synchronously (concat "http://export.arxiv.org/rss/" category)))
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
         new-list
         cross-list
         updated-list)
    (kill-buffer res-buf)
    (with-temp-buffer
      (dolist (title title-list)
        (erase-buffer)
        (insert title)
        (search-backward-regexp "(arXiv:\\([^ ]*\\) \\[\\([^]]*\\)\\]\\([^)]*\\))")
        (cond ((string-equal (match-string 3) " CROSS LISTED")
               (setq cross-list (cons (match-string 1) cross-list)))
              ((string-equal (match-string 3) " UPDATED")
               (setq updated-list (cons (match-string 1) updated-list)))
              (t
               (setq new-list (cons (match-string 1) new-list))))))

    (list new-list cross-list updated-list)))


(defun ep-check-arxiv (category) 
  "Show new entries at the arXiv for CATEGORY."
  (interactive
   (list (read-string (concat "arXiv category [" ep-arxiv-default-category "]: ") nil nil "hep-th")))
                            
  (let* ((ids (ep-arxiv-get-new-ids category))
         (entries-new (ep-arxiv-id-query (car ids)))
         (entries-cross-listed (ep-arxiv-id-query (cadr ids)))
         (entries-updated (ep-arxiv-id-query (caddr ids))))

    (if (not entries-new)
        (message "No new arXiv entries in %s" category)

      (switch-to-buffer (generate-new-buffer (concat "EP arXiv: " category)))
      (ep-ep-insert-main-heading (concat "New arXiv entries for category " category))
      (ep-ep-insert-entries entries-new)

      (ep-ep-insert-sub-heading "Cross listed entries")
      (ep-ep-insert-entries entries-cross-listed)

      (ep-ep-insert-sub-heading  "Updated entries")
      (ep-ep-insert-entries entries-updated)
      (goto-char (point-min))
      (ep-ep-next-entry)
      (ep-ep-mode))))

;;; Connect to Spires

(defun ep-spires-guess-query (key)
  "Guess the Spires query to find KEY."
  (concat

   (cond ((string-match "FIND " key)
          (replace-regexp-in-string " " "+" key))
         ((string-match " " key)
          (concat "FIND+" (replace-regexp-in-string " " "+" key)))
         ((oos-string-match-full "[0-9]\\{4\\}\\.[0-9]\\{4\\}" key) 
          (concat "FIND+EPRINT+" key))        ; Match new arxiv identifier
         ((oos-string-match-full "[0-9]\\{7\\}" key) 
          (concat "FIND+EPRINT+hep-th/" key)) ; Match old arxiv identifier
                                              ; (default to hep-th)
         ((oos-string-match-full "[a-z\\-]+/[0-9]\\{7\\}" key) 
          (concat "FIND+EPRINT+" key))        ; Match old arxiv identifier
         ((oos-string-match-full "[A-Z][a-z]*:[0-9]\\{4\\}[a-z]\\{2\\}[a-z]?" key) 
          (concat "FIND+TEXKEY+" key))        ; Match SPIRES key
         (t
          (concat "FIND+A+" key)))))          ; Default to author search

(defun ep-spires-url (query)
  "Construct an url for a Spires QUERY."
  (concat "http://www-library.desy.de/cgi-bin/spiface/find/hep/www?rawcmd="
          ;;"http://www.slac.stanford.edu/spires/find/hep/www?rawcmd="
          query
          "&FORMAT=wwwbriefbibtex&SEQUENCE="))

(defun ep-spires-query-entries (query)
  "Perform a Apires QUERY. Return a list of entries."
  (let* ((orig-buf (buffer-name))
         (url (ep-spires-url query))
         (query-buf (url-retrieve-synchronously url))
         entries)

    (switch-to-buffer query-buf)
    
    (goto-char (point-min))
    (let* ((start (progn (search-forward "<!-- START RESULTS -->\n" nil 't) (point)))
           (end (progn (search-forward "<!-- END RESULTS -->" nil 't) (- (point) 21))))
      (message "%S" (cons start end))
      (when (< start end)
        (narrow-to-region start end)
        (setq entries (ep-bib-parse-buffer query-buf))))
    (kill-buffer query-buf)
    (switch-to-buffer orig-buf)
    entries))

(defun ep-spires-query (query)
  "Search for QUERY on Spires and desplay the result in a new
Emacs Paper buffer."
  (interactive "sSpires query:")
  (let ((entries (ep-spires-query-entries (ep-spires-guess-query query))))
    (if (not entries)
        (message "No entries found for query %s" query)
    (switch-to-buffer (generate-new-buffer "EP Spires query"))
    (ep-ep-insert-sub-heading (concat "Spires results for query \"" query "\""))
    (ep-ep-insert-entries entries )
    (goto-char (point-min))
    (ep-ep-next-entry)
    (ep-ep-mode))))

(defun ep-spires-update-current-entry ()
  "Update the current entry by getting any mssing fields from
Spires."
  (interactive)
  (let* ((query (or(ep-alist-get-value "=key=" ep-ep-current-entry)
                   (ep-alist-get-value "eprint" ep-ep-current-entry)))
         (entry (when query (car (ep-spires-query-entries (ep-spires-guess-query query))))))
    (cond 
     ((not query) (message "%s" "The current entry has no key and no preprint number"))
     ((not entry) (message "%s" "The current entry was not found on Spires"))
     (t
      (dolist (field entry)
        (ep-alist-insert (car field) ep-ep-current-entry (cdr field)))
      (ep-ep-update-entry ep-ep-current-entry)
      (ep-ep-highlight-entry)))))
