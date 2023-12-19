# Emacs Paper

Emacs Paper is a major mode for browsing and editing BibTeX files, with support for importing data from [arXiv](https://arxiv.org) and [Inspire](https://inspirehep.net/), and a local cache of downloaded PDF files.

The intended workflow for Emacs Paper is that one keeps a main BibTeX files with all references. Emacs Paper can then be used to import entries to this file.

## Installation


Download `emacs-paper.el` to a suitable location and load it using `load`. To get full functionality 

```
(load "path/to/emacs-paper.el")

(set-variable 'ep-main-bib-file "path/to/main/refs.bib")
(set-variable 'ep-pdf-dir "path/to/pdfs/")
(set-variable 'ep-pdf-file "path/to/pdfs/papers.xml")
```

## Usage

The following table shows the basic commands and their key bindings

| Key | Function | Description |
| --- | --- | --- |
| `n` | `ep-next-entry-recenter` | Next entry |
| `p` | `ep-previous-entry-recenter` | Previous entry |
| `M-p` | `ep-scroll-down` | Scroll down |
| `SPC` | `ep-scroll-up` | Scroll up |
| `e` | `ep-edit-entry` | Edit current entry |
| `k` | `ep-kill-entry` | Kill the current entry. |
| `w` | `ep-copy-entry` | Copy the current entry. |
| `y` | `ep-yank-entry` | Create a new entry from BibTeX entry on top of kill-ring |
| `C-c C-e` | `ep-new-entry` | Create a new entry in the current buffer and edit it. |
| `u` | `ep-inspire-update-entry` | Update current entry with data from Inspire |
| `a` | `ep-arxiv-update-entry` | Update current entry by getting any missing fields (including abstract and comments) from arXiv. |
| `b` | `ep-bib-find-file` | Open current file as a raw BibTeX file |
| `I` | `ep-import-marked-entries` | Import all marked entries to the main Emacs Paper buffer.  |
| `g` | `ep-goto` | Find this entry online. Query the user about how to look for the entry. |
| `RET` | `ep-goto-pdf` | Open PDF file, downloading it first id necessary |
| `f` | `ep-search` | Search the main Emacs Paper buffer and Inspire. |
| `i` | `ep-import-entry` | Import current entry to main Emacs Paper buffer |
| `l` | `ep-inspire-latex-entry` | Copy the LaTeX entry from Inspire corresponding to current entry to the kill ring.|
| `m` | `ep-mark-entry` | Mark or unmark the current entry |
| `o` | `ep-sort-entries` | Sort entries. |
| `r` | `ep-regexp` | Search current Emacs Paper buffer using a regexp |
| `s` | `ep-bib-save-file` | Save BibTeX file |
| `C-/` |`ep-undo` | Undo |
| `q` | `ep-quit` | Close Emacs Paper buffer |
| | `ep-bib-load-file`| Load a BibTeX file and display it in a new Emacs Paper buffer. |
| | `ep-check-arxiv` | Show new entries from arXiv. Asks for a category (default is set in `ep-arxiv-default-category`) |
| | `ep-add-pdf` | Manually associate a PDF file to an entry. |


When editing an entry (by pressing `e` on an entry) the entry is opened in a new buffer in a version of bibtex-mode. To close the edit buffer and save any edits, press `C-c C-c` (`ep-ep-edit-done`). To close without editing press `C-g` (`ep-ep-edit-quit`).

To edit multiple entries, open the raw BibTeX file press `b`. When the file is close Emacs Paper asks if it should reload its buffer to pick up any changes.

## Customisation

There are a couple of variables that can be customised. Here are some of the more important ones, for a full list see the customisation menu

| Variable | Description |
| --- | --- |
| `ep-main-bib-file` | Path to the main BibTeX file |
| `ep-pdf-dir` | Directory for storing PDF files. |
| `ep-pdf-file` | File used by Emacs Paper for associating PDF files with BibTeX entries. |
| `ep-arxiv-default-category` | Main arXiv category (default is `hep-th`) |
| `ep-open-pdf-cmd` | Shell commands used to open PDF files. On Linux `xdg-open \"%s\"` works, on Mac try `open \"%s\"` |