;;; org-book-note.el --- Better reading note experience  -*- lexical-binding: t; -*-

;; Author: Yuchen Li
;; Url: https://github.com/yuchen-lea/org-book-note

;;; Commentary:
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;;;; Requirements

(require 'bookmark)

;;;; Customization

(defgroup org-book-note nil
  "Help you manage book notes in org-noter."
  :prefix "org-book-note-"
  :group 'org)

(defcustom org-book-note-image-dir org-directory
  "Directory to store extracted note images."
  :type 'directory)


(defcustom org-book-note-image-zoom-factor 4.0
  "Default zoom factor for exported images."
  :type 'number)

(defcustom org-book-note-annot-template nil
  "Template for annotation items using mako template syntax.

If this is nil, the default template from pdfhelper will be used.
For more details on the template syntax, see:
https://github.com/yuchen-lea/pdfhelper"
  :type 'string)

(defcustom org-book-note-toc-template nil
  "Template for table of contents items using mako template syntax.

If this is nil, the default template from pdfhelper will be used.
For more details on the template syntax, see:
https://github.com/yuchen-lea/pdfhelper"
  :type 'string)

(defcustom org-book-note-bib-files nil
  "List of paths to .bib files to find cite key."
  :type '(repeat file))


;;;; Variables
;;;; Commands


(defun org-book-note-check-pdfhelper-version ()
  "Check if the pdfhelper command-line program exists and its version is >= 2.3.1."
  (interactive)
  (let ((pdfhelper-path (executable-find "pdfhelper"))
        (required-version "2.3.1"))
    (if (not pdfhelper-path)
        (error "pdfhelper command-line program is not found. Please install it from https://github.com/yuchen-lea/pdfhelper")
      (let ((current-version (string-trim (shell-command-to-string "pdfhelper --version"))))
        (if (version< current-version required-version)
            (error "Your pdfhelper version (%s) is outdated. Please upgrade to the latest version from https://github.com/yuchen-lea/pdfhelper"
                     current-version)
          (message "pdfhelper version is sufficient."))))))

;;;;; PDF

;;;###autoload
(defun org-book-note-insert-pdf-annots (pdf-path)
  "Insert annotations of a PDF file using pdfhelper.
If PDF-PATH is not provided, prompt the user to select a PDF file."
  (interactive (list (read-file-name "Select a PDF file: " nil
                                     nil t)))
  (org-book-note-check-pdfhelper-version)
  (unless (and (f-exists? pdf-path)
               (string= (file-name-extension pdf-path)
                        "pdf"))
    (error "The provided file is not a valid PDF"))
  ;; Execute the shell command asynchronously
  (bookmark-set "org-book-note-temp-bookmark")
  (let* ((output-buffer (generate-new-buffer "*pdfhelper-output*"))
         (async-shell-command-display-buffer nil)
         (proc (progn
                 (async-shell-command (org-noter-note--pdfhelper-export-annot-cmd
                                       pdf-path)
                                      output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc
                              #'(lambda (process signal)
                                  (when (memq (process-status process)
                                              '(exit signal))
                                    (bookmark-jump "org-book-note-temp-bookmark")
                                    (sleep-for 1)
                                    (goto-char (org-element-property :end (org-element-context)))
                                    (yank)
                                    (bookmark-delete "org-book-note-temp-bookmark")
                                    (shell-command-sentinel process signal))))
      (message-box "No process running."))))

(defun org-noter-note--pdfhelper-export-annot-cmd (pdf-path)
  (let* ((test-p (y-or-n-p "Run a test first?"))
        ;; (ocr-p (y-or-n-p "OCR on picture?"))
        (with-toc (y-or-n-p "With Toc?"))
        ;; (ocr-service (if ocr-p (ido-completing-read "Pick ocr services:" '("paddle" "ocrspace")) ""))
        ;; (ocr-language (if (string= ocr-service "ocrspace") (ido-completing-read "Pick ocr language:" '("zh-Hans" "zh-Hant" "en" "ja")) ""))
        (zoom-factor (read-number "Enter image zoom factor: " org-book-note-image-zoom-factor)))
    (mapconcat #'identity
               (list "pdfhelper export-annot"
                     (if with-toc "--with-toc" "")
                     (format "--image-zoom %s"
                             (if (> zoom-factor 0) zoom-factor org-book-note-image-zoom-factor))
                     ;; (if ocr-p
                     ;;     (format "--ocr-service '%s'" ocr-service)
                     ;;   "")
                     ;; (format "--ocr-language '%s'" ocr-language)
                     (format "--annot-image-dir '%s'" org-book-note-image-dir)
                     (if org-book-note-bib-files (format "--bib-path %s" (mapconcat (lambda (item) (format "'%s'" item)) org-book-note-bib-files " ")))
                     (if org-book-note-annot-template (format "--annot-list-item-format '%s'" org-book-note-annot-template))
                     (if org-book-note-toc-template (format "--toc-list-item-format '%s'" org-book-note-toc-template))
                     (if test-p "--run-test")
                     (format "'%s'" pdf-path)
                     ;; TODO cross-platform
                     (format "| %s"
                             (cond
                              ((eq system-type 'gnu/linux) "xclip")
                              ((eq system-type 'darwin) "pbcopy")
                              ((memq system-type
                                     '(cygwin windows-nt ms-dos)) "clip.exe"))))
               " ")))
;;;; Footer
(provide 'org-book-note)
;;; org-book-note.el ends here
