;;;; Emacs Paper
;; 
;; Emacs paper for managing references

;;; Set up variables
(defcustom ep-main-bib-file 
;;  "c:/Users/Olof/Documents/Jobb/Notes/References/refs.bib"
  "c:/Users/Olof/Documents/emacs-paper/test.bib"
;;  "/Users/olof/Documents/emacs-paper/test.bib"
  "Main BibTeX database used by Emacs Paper."
  :type 'string)

(defcustom ep-arxiv-default-category "hep-th"
  "Default arXiv category to use when checking for new articles."
  :type 'string)

(defcustom ep-highlight-color 
  ;"honeydew"
  "gray15"
  "Background color of highlighted entry."
  :type 'color)

(defvar ep-main-buffer nil
  "Maine Emacs Paper buffer")

(defvar ep-bib-fields
  '("author" "title" "journal" "volume" "number" "publisher" "year" "month" 
    "edition" "address" "pages" "eprint" "archivePrefix" "primaryClass"
    "doi" "printed" "SLACcitation" "note" "ep-tags")
  "BibTeX fields saved by Emacs Paper. The fields are inserted in
  the order of appearance in the list.")

(defvar ep-ep-common-tags
  '("Printed" "Print this"))

(defvar ep-ep-highlight-overlay nil
  "Overlay used to highlight the current entry.")

(defvar ep-ep-current-entry nil
  "The current active entry in the buffer.")

(defvar ep-ep-visited-file nil)

(defvar ep-ep-file-buffers nil)

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

(defun ep-bib-load-file (file)
  (interactive "FLoad BibTeX file: ")

  (let* ((file-buf (find-file file))
         (entries (ep-bib-parse-buffer file-buf)))
    (if (not entries)
        (message "No BibTeX entries found in %s" file)
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

(defun ep-bib-save-file (&optional buffer)
  (interactive)

  (when buffer (set-buffer buffer))

  (if (and ep-ep-visited-file (not (buffer-modified-p)))
      (message "(No changes need to be saved)")
    (let ((entries (ep-ep-extract-entries))
          no-key)
      (while (and entries (not no-key))
        (unless (ep-field-value "=key=" (car entries))
          (setq no-key (car entries)))
        (pop entries))
      (when no-key
        (goto-char (car (ep-ep-entry-boundaries no-key)))
        (error "Entry has no key")))

    (save-excursion
      (let ((filename (or ep-ep-visited-file (read-file-name "Save to file:")))
            (entries (ep-ep-extract-entries)))
        (cond
         ((not filename) (error "Empty filename"))
         ((not entries) (error "The buffer contain no entries"))
         (t
          (ep-bib-save-entries entries filename)
          (set-buffer-modified-p nil)))))))

(defun ep-ep-kill-buffer-query-function ()
  (if (or (not ep-ep-visited-file) (not (buffer-modified-p)))
      t
    (yes-or-no-p (concat "Buffer " (buffer-name (current-buffer)) " modified; kill anyway? "))))

(defun ep-ep-kill-emacs-query-function ()
  (let ((all-saved t))
    (dolist (buffer ep-ep-file-buffers)
      (when (and (buffer-live-p buffer) (buffer-modified-p buffer))
        (if (y-or-n-p (concat "Save EP buffer " (buffer-name buffer) "?"))
            (ep-bib-save-file buffer)
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
        
        (while (search-forward "@" nil t)
          (backward-char)
          (let* ((entry (bibtex-parse-entry t)))
            (dolist (field entry)
              (setcdr field (ep-cleanup-whitespace (cdr field))))
            (push entry entries))
          (progress-reporter-update progress (point)))
        (progress-reporter-done progress)
        (nreverse entries)))))

(defun ep-bib-save-entries (entries file)
  "Save the BibTeX ENTRIES to FILE."
  (save-current-buffer
    (find-file file)
    (erase-buffer)
    (ep-bib-format-entries entries)
    (save-buffer)
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
                                        " Do you want to continue?"))))
      (erase-buffer)
      (insert "This file was ceated by Emacs Paper.\n\n")
      (ep-bib-format-entries entries)
      (setq did-write t))
    did-write))

(defun ep-bib-format-entries (entries)
  "Insert ENTRIES into current buffer."
  (let ((progress (make-progress-reporter "Insering entries..." 0 (length entries)))
        (counter 0))
    (dolist (entry entries)
      (ep-bib-format-entry entry)
      (setq counter (+ 1 counter))
      (progress-reporter-update progress counter)
      (insert "\n\n"))
    (progress-reporter-done progress)))

(defun ep-bib-format-entry (entry)
  "Insert ENTRY in current buffer."
  (insert "@" (ep-field-value "=type=" entry) (bibtex-entry-left-delimiter))
  (insert (ep-field-value "=key=" entry))
  (dolist (field-name ep-bib-fields)
    (let ((field-value (ep-field-value field-name entry)))
      (if field-value
          (bibtex-make-field (list field-name nil field-value nil)))))
  (insert "\n")
  (insert (bibtex-entry-right-delimiter)))

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
    (or (ep-ep-insert-non-nil (ep-field-value "=key=" entry) "\n")
        (ep-ep-insert-non-nil (ep-field-value "eprint" entry)  " "
                              "[" (ep-field-value "primaryClass" entry) "]" "\n")
        (ep-ep-insert-non-nil " " (ep-field-value "eprint" entry)  "\n"))
    
    (ep-ep-insert-non-nil (ep-ep-propertize-non-nil 
                           (ep-field-value "title" entry) 
                           'face '(:weight bold :slant italic :height 1.1)) "\n")
    (ep-ep-insert-non-nil (ep-field-value "author" entry))

    (when (or (ep-field-value "year" entry) 
              (ep-field-value "journal" entry)
              (and (ep-field-value "=key=" entry) (ep-field-value "eprint" entry)))
      (insert "\n"))
    
    (if (string-equal "JHEP" (ep-field-value "journal" entry))
        (ep-ep-insert-non-nil (ep-ep-propertize-non-nil 
                               (ep-field-value "journal" entry) 'face 'italic ) " " 
                               (ep-ep-propertize-non-nil 
                                (ep-ep-concat-non-nil 
                                 (ep-ep-substring-non-nil (ep-field-value "year" entry) 2)
                                 (ep-field-value "volume" entry))
                                'face 'bold) ", "
                               (ep-field-value "pages" entry) " ")
      (ep-ep-insert-non-nil (ep-ep-propertize-non-nil (ep-field-value "journal" entry) 
                                                      'face 'italic ) " "
                            (ep-ep-propertize-non-nil (ep-field-value "volume" entry) 
                                                      'face 'bold) ", "
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
    (ep-ep-insert-non-nil "Tags: " (ep-field-value "ep-tags" entry) "\n")

    (when (ep-field-value "abstract" entry)
      (ep-ep-insert-non-nil "Comments: " (ep-field-value "arxiv-comment" entry) "\n")
      (insert "\n")
      (insert (ep-field-value "abstract" entry)))
    
    (put-text-property start (point) :ep-entry entry)))

(defun ep-ep-format-entries (entries)
  "Insert ENTRIES in current buffer."
  (dolist (entry entries)
    (ep-ep-format-entry entry)))

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

(defun ep-ep-next-entry-recenter (&optional n)
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

(defun ep-ep-previous-entry-recenter (&optional n)
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

(defun ep-ep-section-entries-boundaries ()
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
    (cons start  end)))

(defun ep-sort-entries (&optional key interactive)
  (interactive "i\np")
  (let ((key (or key 
                 (if interactive
                     (completing-read "Sort by [BibTeX key]: " ep-bib-fields)
                     "=key="))))
    (when (string-equal key "")
      (setq key "=key="))
    (ep-ep-redraw-entries (lambda (entries)
                            (sort entries (lambda (entry-a entry-b)
                                            (string-lessp (ep-alist-get-value key entry-a)
                                                          (ep-alist-get-value key entry-b))))))))

(defun ep-ep-redraw-entries (&optional func)
  "Redraw all entries. FUNC should be a function taking as a
single argument a list of entries, and returning a list of
entries to draw. If FUNC is nil, it defaults to `identity'."
  (let ((func (or func 'identity))
        (current-entry ep-ep-current-entry))
    (goto-char (point-min))
    (toggle-read-only -1)
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
    (goto-char (car (ep-ep-entry-boundaries current-entry)))
    (ep-ep-highlight-entry)))

(defun ep-ep-filter-entries (entries filters)
  "Filter ENTRIES. FILTERS should be a list of cons-cells (BibTeX-field . regexp)."
  (let (res-entries match)
    (dolist (entry entries)
      (setq match t)
      (dolist (filter filters)
        (cond
         ((listp filter)
          (let ((field-val (ep-alist-get-value (car filter) entry))
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
                   (completing-read "Tag: " ep-ep-common-tags))))

      (let* ((buffer-name (buffer-name))
             (entries (ep-ep-filter-entries 
                       (ep-ep-extract-entries) (list (cons "ep-tags" tag)))))
        (ep-ep-new-buffer (concat (buffer-name) ":" tag)
          (ep-ep-insert-main-heading (concat "Entries in '" buffer-name "' with tag '" tag "'"))
          (ep-ep-format-entries entries)))))

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

  (set (make-variable-buffer-local 'ep-ep-highlight-overlay) (make-overlay (point) (point)))
  (overlay-put ep-ep-highlight-overlay 
               'face 
               (cons 'background-color ep-highlight-color))

  (add-hook (make-variable-buffer-local 'post-command-hook) 'ep-ep-post-command-hook nil t)

  (ep-ep-highlight-entry))

(define-key ep-ep-mode-map "n" 'ep-ep-next-entry-recenter)
(define-key ep-ep-mode-map "p" 'ep-ep-previous-entry-recenter)

(define-key ep-ep-mode-map "m" 'ep-mark-entry)
(define-key ep-ep-mode-map "i" 'ep-import-entry)
(define-key ep-ep-mode-map "I" 'ep-import-marked-entries)

(define-key ep-ep-mode-map "e" 'ep-edit-entry)
(define-key ep-ep-mode-map "t" 'ep-add-tag)
(define-key ep-ep-mode-map "T" 'ep-remove-tag)

(define-key ep-ep-mode-map "o" 'ep-sort-entries)

(define-key ep-ep-mode-map "u" 'ep-spires-update-entry)
(define-key ep-ep-mode-map "a" 'ep-arxiv-update-entry)
(define-key ep-ep-mode-map "f" 'ep-search)
(define-key ep-ep-mode-map "r" 'ep-regexp)

(define-key ep-ep-mode-map "s" 'ep-bib-save-file)
(define-key ep-ep-mode-map "q" 'ep-quit)

(define-key ep-ep-mode-map "g" 'ep-goto)

(define-derived-mode  ep-ep-edit-mode bibtex-mode "EP BibTeX edit"
  "Major mode for editing Emacs Paper BibTeX entries.
\\\{ep-ep-edit-mode-map}")

(define-key ep-ep-edit-mode-map "\C-c\C-c" 'ep-ep-edit-done)
(define-key ep-ep-edit-mode-map "\C-g" 'ep-ep-edit-quit)

(defmacro ep-ep-new-buffer (name &rest body)
  (declare (indent defun))
  `(progn
     (let ((buf (generate-new-buffer ,name)))
       (switch-to-buffer buf)
       (progn ,@body)
       (set-buffer-modified-p nil)
       (toggle-read-only 1)
       (goto-char (point-min))
       (ep-ep-next-entry)
       (ep-ep-mode)
       buf)))

(defun ep-ep-entry-at-point (&optional point)
  "Return the entry at POINT. If POINT is nil, use `point;"
  (let ((point (if point
                   point
                 (point))))
    (get-text-property point :ep-entry)))

(defun ep-ep-extract-entries (&optional buffer)
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (goto-char (point-min))
    (let (entries)
      (while (ep-ep-next-entry)
        (push (ep-ep-entry-at-point) entries))
      (nreverse entries))))

(defun ep-ep-extract-marked-entries (&optional buffer)
  (if buffer
      (set-buffer buffer))
  (let ((overlays (overlays-in (point-min) (point-max)))
        entries)
    (dolist (overlay overlays)
      (when (overlay-get overlay :ep-mark)
        (push (ep-ep-entry-at-point (overlay-start overlay)) entries)))
    (nreverse entries)))

;; Entry editing

(defun ep-ep-update-entry (entry)
  "Redraw ENTRY."
  (let* ((highlight (eq entry ep-ep-current-entry))
         (boundaries (ep-ep-entry-boundaries entry))
         (start (car boundaries))
         (end (cdr boundaries)))
    (save-excursion
      (toggle-read-only -1)
      (goto-char start)
      (delete-region (- start 1)
                     end)
      (ep-ep-format-entry entry)
      (toggle-read-only 1))
    
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

(defun ep-edit-entry (&optional entry)
  "Edit ENTRY as a BibTeX entry. Default to editing the current entry."
  (interactive)
  (let* ((entry  (or entry ep-ep-current-entry))
         (highlight (eq entry ep-ep-current-entry))
         (edit-buffer (generate-new-buffer "EP edit entry"))
         new-entry)
    (switch-to-buffer edit-buffer)
    (buffer-disable-undo)
    (ep-bib-format-entry entry)
    (goto-char 0)
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (ep-ep-edit-mode)
    (when (catch 'ep-edit-quit
            (recursive-edit))
      (setq new-entry (car (ep-bib-parse-buffer edit-buffer))))
    (kill-buffer edit-buffer)

    (if  (not new-entry)
        (message "Canceled")
      (ep-alist-clear entry)
      (dolist (field new-entry)
        (ep-alist-set (car field) entry (cdr field)))

      (ep-ep-update-entry entry))))

(defun ep-edit-tags (&optional entry)
  (interactive)
  (let* ((entry (or entry ep-ep-current-entry))
         (tags (ep-field-value "ep-tags" entry))
         (new-tags (read-from-minibuffer "Tags: " tags)))
    (when (string-equal "" new-tags)
      (setq new-tags nil))
    (ep-alist-set "ep-tags" entry new-tags)
    (ep-ep-update-entry entry)))

(defun ep-add-tag (&optional entry)
  (interactive)
  (let* ((entry (or entry ep-ep-current-entry))
         (tags-val (ep-field-value "ep-tags" entry))
         (tags (and tags-val (split-string tags-val ",")))
         (tag (completing-read "Add tag: " ep-ep-common-tags nil nil))
         new-tags)
    (if (member tag tags)
        (message "Entry already tagged with '%s'" tag)
      (setq new-tags (mapconcat 'identity (cons tag tags) ","))
      (when (string-equal "" new-tags)
        (setq new-tags nil))
      (ep-alist-set "ep-tags" entry new-tags)
      (ep-ep-update-entry entry))))


(defun ep-remove-tag (&optional entry)
  (interactive)
  (let* ((entry (or entry ep-ep-current-entry))
         (tags-val (ep-field-value "ep-tags" entry))
         (tags (and tags-val (split-string tags-val ",")))
         tag new-tags)
    (if (not tags)
        (message "Entry has no tags")
      (setq tag (completing-read "Remove tag: " tags nil t))
    (cond
     ((string-equal tag "")
      (message "Abort"))
     ((not (member tag tags))
        (message "Entry is not tagged with '%s' (How could this happen?)" tag))
     (t
      (setq new-tags (mapconcat 'identity (delete tag tags) ","))
      (when (string-equal "" new-tags)
        (setq new-tags nil))
      (ep-alist-set "ep-tags" entry new-tags)
      (ep-ep-update-entry entry))))))


(defun ep-mark-entry (&optional entry)
  "Mark ENTRY if it is not marked, otherwise unmark it."
  (interactive)
  (save-excursion
    (let* ((entry (or entry ep-ep-current-entry))
           (boundaries (ep-ep-entry-boundaries entry))
           (start (car boundaries))
           (end (cdr boundaries))
           (overlays (overlays-at start))
           marked)
      (while (and overlays (not marked))
        (when (overlay-get (car overlays) :ep-mark)
          (setq marked (car overlays)))
        (pop overlays))
      (cond 
       (marked 
        (delete-overlay marked)
        (message "Entry unmarked")
        nil)
       (t        
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'face 'bold)
          (overlay-put overlay :ep-mark t)
          (message "Entry marked"))
        t)))))

(defun ep-import-entry (&optional entry)
  (interactive)
  
  (let ((entry (or entry ep-ep-current-entry)))
    (cond 
     ((not entry) (error "No entries to save"))
     ((not ep-main-buffer) (error "Main buffer is not loaded"))
     (t
      (set-buffer ep-main-buffer)
      (let ((entries (ep-ep-extract-entries))
            old-entry)
        (while (and entries (not old-entry))
          (when (or (eq entry (car entries))
                    (equal (ep-alist-get-value "=key=" entry) 
                           (ep-alist-get-value "=key=" (car entries)))
                    (equal (ep-alist-get-value "eprint" entry) 
                           (ep-alist-get-value "eprint" (car entries))))
            (setq old-entry entry))
          (pop entries))
        (cond 
         (old-entry
          (message "Entry already exists in '%s'" (buffer-name ep-main-buffer))
          nil)
         (t
          (goto-char (point-max))
          (toggle-read-only -1)
          (ep-ep-format-entry entry)
          (toggle-read-only 1)
          (message "Entry saved to '%s'" (buffer-name ep-main-buffer)))))))))
         
(defun ep-import-marked-entries ()
  (interactive)
  (let ((marked-entries (ep-ep-extract-marked-entries))
        (saved 0))
    (dolist (entry marked-entries)
      (when (ep-import-entry entry)
        (incf saved)))
    (message "Saved %d entries" saved)))

;;; Find entry online

(defun ep-goto ()
  "Find this entry online. Query the user about how to look for the entry."
  (interactive)
  (let* ((collection '(("arXiv abstract" . 1) ("DOI" . 2) ("Spires record" . 3) ("PDF" . 4)))
         (completion-ignore-case t)
         (answer (completing-read "Go to: " collection nil t)))
    (case (cdr (assoc answer collection))
      (1 (ep-goto-arxiv-abstract))
      (2 (ep-goto-doi))
      (3 (ep-goto-spires))
      (4 (ep-goto-arxiv-pdf))
      (otherwise (error "Cannot go to '%s'" answer)))))

(defun ep-goto-arxiv-abstract ()
  "Go to the arXiv abstract page of the current entry."
  (interactive)
  (let* ((entry ep-ep-current-entry)
         (url (ep-ep-concat-non-nil "http://arxiv.org/abs/" (ep-alist-get-value "eprint" entry))))
    (if url
        (browse-url url)
      (message "There is no preprint number for this entry. Trying at Spires.")
      (ep-goto-spires))))

(defun ep-goto-arxiv-pdf ()
  "Go to the arXiv PDF of the current entry."
  (interactive)
  (let* ((entry ep-ep-current-entry)
         (url (ep-ep-concat-non-nil "http://arxiv.org/pdf/" (ep-alist-get-value "eprint" entry))))
    (if url
        (browse-url url)
      (message "There is no preprint number for this entry. Trying using DOI.")
      (ep-goto-doi))))

(defun ep-goto-spires ()
  "Go to the Spires record of the current entry."
  (interactive)
  (let* ((entry ep-ep-current-entry)
         (query (or (ep-alist-get-value "=key=" entry)
                    (ep-alist-get-value "eprint" entry)))
         (url (when query (ep-spires-url (ep-spires-guess-query query) "www"))))
    (if url
        (browse-url url)
      (message "There is no preprint number for this entry. Trying using DOI.")
      (ep-goto-doi))))
 
(defun ep-goto-doi ()
  "Follow the DOI of the current entry."
  (interactive)
  (let* ((entry ep-ep-current-entry)
         (url (ep-ep-concat-non-nil "http://dx.doi.org/" (ep-alist-get-value "doi" entry))))
    (if url
        (browse-url url)
      (message "There is no DOI for the current entry."))))


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
                 (comment-node (car (xml-get-children entry 'arxiv:comment)))
                 (title (car (xml-node-children title-node)))
                 (summary (car (xml-node-children summary-node)))
                 (id (car (xml-node-children id-node)))
                 (category (xml-get-attribute category-node 'term))
                 (comment (car (xml-node-children comment-node)))
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
                                     (list (list (cons "eprint" id) 
                                                 (cons "author" author) 
                                                 (cons "title" title) 
                                                 (cons "abstract" summary) 
                                                 (cons "arxiv-comment" comment)
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
        (setq entries (ep-arxiv-parse-atom-buffer xml-file-buf))

        (kill-buffer xml-file-buf)
        (delete-file xml-file)

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
        (search-backward-regexp "(arXiv:\\([^ ]*\\) \\(\\[[^]]*\\]\\)? ?\\([^)]*\\))" nil t)
        (cond ((string-equal (match-string 3) "CROSS LISTED")
               (setq cross-list (cons (match-string 1) cross-list)))
              ((string-equal (match-string 3) "UPDATED")
               (setq updated-list (cons (match-string 1) updated-list)))
              (t
               (setq new-list (cons (match-string 1) new-list))))))

    (list (nreverse new-list)
          (nreverse cross-list) 
          (nreverse updated-list))))

(defun ep-arxiv-update-entry (&optional entry overwrite)
  "Update ENTRY by getting any missing fields from
arXiv."
  (interactive "i\nP")
  (let* ((entry (or entry ep-ep-current-entry))
         (highlight (eq entry ep-ep-current-entry))
         (eprint (ep-alist-get-value "eprint" entry))
         (arxiv-entry (when eprint (car (ep-arxiv-id-query (list eprint))))))
    (cond 
     ((not eprint) (message "%s" "The current entry has no preprint number"))
     (t
      (dolist (field arxiv-entry)
        (if (not overwrite)
            (ep-alist-insert (car field) entry (cdr field))
          (ep-alist-set (car field) entry (cdr field))))
      (ep-ep-update-entry entry)
      (message "Entry updated")))))

(defun ep-check-arxiv (category) 
  "Show new entries at the arXiv for CATEGORY."
  (interactive
   (list (read-string (concat "arXiv category [" ep-arxiv-default-category "]: ") 
                      nil nil "hep-th")))
                            
  (let* ((ids (ep-arxiv-get-new-ids category))
         (entries-new (ep-arxiv-id-query (car ids)))
         (entries-cross-listed (ep-arxiv-id-query (cadr ids)))
         (entries-updated (ep-arxiv-id-query (caddr ids))))

    (if (not entries-new)
        (message "No new arXiv entries in %s" category)

      (ep-ep-new-buffer (concat "EP arXiv: " category)
        (ep-ep-insert-main-heading (concat "New arXiv entries for category " category))
        (ep-ep-format-entries entries-new)

        (ep-ep-insert-sub-heading "Cross listed entries")
        (ep-ep-format-entries entries-cross-listed)

        (ep-ep-insert-sub-heading  "Updated entries")
        (ep-ep-format-entries entries-updated)))))

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

(defun ep-spires-url (query &optional format)
  "Construct an url for a Spires QUERY."
  (let ((format (or format "wwwbriefbibtex")))
    (concat "http://www-library.desy.de/cgi-bin/spiface/find/hep/www?rawcmd="
            ;;"http://www.slac.stanford.edu/spires/find/hep/www?rawcmd="
            query
            "&FORMAT=" format "&SEQUENCE=")))

(defun ep-spires-extract-entries (query-buf)
  "Extract entries from a buffer resulting from a Spires query in
QUERY-BUF. Return a list of entries. Kill QUERY-BUF after the
entries are extracted."
  (save-current-buffer
    (let* (entries)

      (switch-to-buffer query-buf)
      
      (goto-char (point-min))
      (let* ((start (progn (search-forward "<!-- START RESULTS -->\n" nil 't) (point)))
             (end (progn (search-forward "<!-- END RESULTS -->" nil 't) (- (point) 21))))
        (when (< start end)
          (narrow-to-region start end)
          (setq entries (ep-bib-parse-buffer query-buf))))
      (kill-buffer query-buf)
      entries)))

;; Searching locally and in Spires

(defun ep-spires-query-callback (status buf)
  "Insert entries returned by a Spires query. Called by `url-retrieve' in `ep-search'."
  (let ((entries (ep-spires-extract-entries (current-buffer)))
        point)
    (switch-to-buffer buf)
    (setq point (point))
    (toggle-read-only -1)
    (goto-char (point-max))
    (ep-ep-format-entries entries)
    (toggle-read-only 1)
    (goto-char point)))

(defun ep-search (query)
  "Search for QUERY in the main Emacs Paper buffer and in Spires."
  (interactive "sSearch query: ")
  (let* ((spires-query (ep-spires-guess-query query))
         (url (ep-spires-url spires-query)))
    (ep-ep-new-buffer (concat "EP search results: " query)
       (ep-ep-insert-main-heading (concat "Search results for '" query "'"))
       (ep-ep-insert-sub-heading "Local results")
       (let* ((ep-query (ep-ep-search-parse-query spires-query))
              (entries (ep-ep-filter-entries (ep-ep-extract-entries ep-main-buffer) ep-query)))
         (ep-ep-format-entries entries))
       (ep-ep-insert-sub-heading "Spires results"))
    (url-retrieve url 'ep-spires-query-callback (list (current-buffer)))
    ))

(defun ep-ep-search-parse-query (query)
  "Parse the Spires formatted QUERY and returns a list of
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
       ((string-match "^eprint " elem)
        (push (cons "eprint" (substring elem 7)) result))
       ((string-match "^texkey " elem)
        (push (cons "=key=" (substring elem 7)) result))))
    result))

(defun ep-spires-update-entry (&optional entry overwrite)
  "Update ENTRY by getting any missing fields from Spires. If
ENTRY is nil, default to the current entry. If OVERWRITE is
non-nil, replace any exisitng fields."
  (interactive "i\nP")
  (let* ((entry (or entry ep-ep-current-entry))
         (highlight (eq entry ep-ep-current-entry))
         (query (or(ep-alist-get-value "=key=" entry)
                   (ep-alist-get-value "eprint" entry)))
         (spires-entry (when query 
                         (car (ep-spires-query-entries (ep-spires-guess-query query))))))
    (cond 
     ((not query) (message "%s" "The current entry has no key and no preprint number"))
     ((not spires-entry) (message "%s" "The current entry was not found on Spires"))
     (t
      (dolist (field spires-entry)
        (if (not overwrite)
            (ep-alist-insert (car field) entry (cdr field))
          (ep-alist-set (car field) entry (cdr field))))
      (ep-ep-update-entry entry)
      (message "Entry updated")))))

;;  Main buffer

(defun ep-main ()
  (interactive)
  (cond 
   ((not (buffer-live-p ep-main-buffer)) (setq ep-main-buffer (ep-bib-load-file ep-main-bib-file)))
   ((and (y-or-n-p "Emacs Paper main buffer is already open. Reread the main BibTeX file? (This will kill the buffer).")
         (kill-buffer ep-main-buffer))
    (setq ep-main-buffer (ep-bib-load-file ep-main-bib-file)))
   (t (switch-to-buffer ep-main-buffer))))
  

(defun ep-quit ()
  (interactive)

  (let ((buffer (current-buffer)))
    (when (kill-buffer buffer)
      (message "Closing Emacs Paper buffer")
      (when (eq buffer ep-main-buffer)
        (setq ep-main-buffer nil)))))