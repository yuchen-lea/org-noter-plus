;;; org-noter-plus.el --- extract outline and annotations from doc  -*- lexical-binding: t; -*-

;; Author: Yuchen Li
;; Url: https://github.com/yuchen-lea/org-noter-plus

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
(require 'org-noter)

;;;; Customization

(defgroup org-noter-plus nil
  "Help you take notes of pdf and epub with org-noter."
  :group 'org
  :prefix "org-noter-plus-")

(defcustom org-noter-plus-image-dir org-directory
  "The directory to save extracted square note images."
  :type 'string
  )

(defcustom org-noter-plus-pdf-link-prefix "pdf"
  "Prefix for pdf link. Please keep consistent with `org-pdftools-link-prefix'"
  :type 'string
  )

(defcustom org-noter-plus--toc-script "/Users/yuchen/Works/personal/pdfhelper/pdfhelper.py"
  "Pdfhelper Script"
  :type 'string
  )


;;;; Commands
;;;;; pdf info
(defun org-noter-plus--pdf-skeletion-info (&optional file-or-buffer)
  "Outline and annotations of given pdf file."
  (require 'pdf-tools)
  (require 'cl-seq)
  (let* ((outline (pdf-info-outline file-or-buffer))
         (annots (sort
                  (pdf-info-getannots nil file-or-buffer)
                  'pdf-annot-compare-annotations))
         (skeleton (append outline annots))
         (pdf-image-buffer (get-buffer-create "*temp pdf image*"))
         (file-name (file-name-base file-or-buffer))
         )
    (with-temp-buffer
      (insert-file-contents file-or-buffer)
      (pdf-view-mode)
      (mapc
       (lambda (annot) ;; traverse all annotations
         (let* ((page (assoc-default 'page annot))
                (type (assoc-default 'type annot))
                (id (symbol-name (assoc-default 'id annot)))
                (old-contents (assoc-default 'contents annot))
                (region (assoc-default 'edges annot))
                (text (if (eq type 'text) nil (pdf-info-gettext page region))) ;; get text
                (imagefile (concat org-noter-plus-image-dir file-name "-" id ".png"))
                )
           ;; add text to content
           (setf (alist-get 'contents annot) (cons old-contents text))
           ;; Square annotations are written to images
           (when (eq type 'square)
             (pdf-view-extract-region-image (list region) page (cons 1000 1000) pdf-image-buffer 1)
             (with-current-buffer pdf-image-buffer
               (write-file imagefile))
             )
           )
         )
       annots))
    ;; sort by page number
    (cl-sort skeleton #'< :key (lambda (alist) (alist-get 'page alist))))
  )

;;;;; nov outline data
(defun org-noter-plus--handle-nov-toc-item (ol depth)
  ;; (require 'dom)
  (mapcar (lambda (li)
            (mapcar (lambda (a-or-ol)
                      (pcase-exhaustive (dom-tag a-or-ol)
                        ('a
                         (vector :depth depth
                                 :title (dom-text a-or-ol)
                                 :href (dom-attr a-or-ol 'href)))
                        ('ol
                         (org-noter-plus--handle-nov-toc-item a-or-ol
                                          (1+ depth)))))
                    (dom-children li)))
          (dom-children ol)))

(defun org-noter-plus--nov-outline-info ()
  "Epub outline with nov link."
  (require 'esxml)
  (require 'nov)
  (let* ((toc-path (cdr (aref nov-documents 0)))
         (toc-tree (with-temp-buffer
                     (insert (nov-ncx-to-html toc-path))
                     (replace-regexp "\n"
                                     ""
                                     nil
                                     (point-min)
                                     (point-max))
                     (libxml-parse-html-region (point-min)
                                               (point-max))))
         output-data nov-buffer
         )
    (nov--find-file nov-file-name 0 0)
    (setq nov-buffer (buffer-name))
    (dolist (item
             (flatten-tree (org-noter-plus--handle-nov-toc-item toc-tree 1))
             )
      ;; TODO vector or alist?
      (let ((depth  (aref item 1))
            (title  (aref item 3))
            (url (aref item 5))
            )
        (nov-goto-toc)  ;; without this, raise error "Couldn’t locate document"
        (apply 'nov-visit-relative-file
               (nov-url-filename-and-target url))
        (when (not (integerp nov-documents-index))
          (setq nov-documents-index 0))
        (push (vector title depth nov-documents-index (point)) output-data)
        )
      )
    (kill-buffer nov-buffer)
    (delete-window)
    (nreverse output-data)
    )
  )

;;;;; nov follow link
(defun org-noter-plus--follow-nov-link (path)
  "Follow nov link designated by PATH.
When in org-noter, open the link in noter doc window.
Set this function for nov link after nov.el is loaded."
  (if (string-match "^\\(.*\\)::\\([0-9]+\\):\\([0-9]+\\)$" path)
      (let ((file (match-string 1 path))
            (index (string-to-number (match-string 2 path)))
            (point (string-to-number (match-string 3 path))))
        (if (bound-and-true-p org-noter--session)
            ;; in noter
            (org-noter--with-valid-session
             (let ((doc (org-noter--session-property-text session))
                   )
               (if (string-equal doc file)
                   ;; current file
                   (progn
                     (select-window
                      (org-noter--get-doc-window))
                     (setq nov-documents-index index)
                     (nov-render-document)
                     (goto-char point))
                 ;; not current file
                 (let ((org-link-frame-setup
                        (cl-acons 'file 'find-file-other-frame org-link-frame-setup)))
                   (org-open-file file 1)
                   (setq nov-documents-index index)
                   (nov-render-document)
                   (goto-char point)
                   ))))
          ;; ouside noter
          (nov--find-file file index point))
        )
    (error "Invalid nov.el link")))


;;;;; skeleton

;;;###autoload
(defun org-noter-plus-create-skeleton-list ()
  "Insert skeleton at the end of noter buffer.
If noter doc is pdf: insert pdf outline with annotations,
If noter doc is epub: insert epub outline (nov link)"
  (interactive)
  (org-noter--with-valid-session
   (let* ((doc-file (org-noter--session-property-text session))
          (ast (org-noter--parse-root))
          (top-level (org-element-property :level ast))
          (level (+ 1 top-level)))
     (cond
      (
       ;; extract pdf outline with annotation
       (eq (org-noter--session-doc-mode session) 'pdf-view-mode)
       (with-current-buffer (org-noter--session-notes-buffer session)
           (widen)
           (save-excursion
             (goto-char (org-element-property :end ast))
             (insert (format
                      "%s Skeleton\n"
                      (make-string level ?*)))
             (org-noter-plus-insert-annots-by-pdf-tool doc-file)
             (setq ast (org-noter--parse-root))
             (org-noter--narrow-to-root ast)
             (goto-char (org-element-property :begin ast))
             )
           )
       )
      ;; extract epub outline
      ((eq (org-noter--session-doc-mode session) 'nov-mode)
       (let ((output-data (org-noter-plus--nov-outline-info))
             title depth doc point)
         (with-current-buffer (org-noter--session-notes-buffer session)
           (widen)
           (save-excursion
             (goto-char (org-element-property :end ast))
             (insert (format
                      "%s Skeleton\n"
                      (make-string level ?*)))
             (dolist (data output-data)
               (setq title (aref data 0)
                     depth (aref data 1)
                     doc (aref data 2)
                     point (aref data 3))

               (insert (format
                        "%s- [[nov:%s::%d:%d][%s]]\n"
                        (make-string depth ? )
                        doc-file
                        doc
                        point
                        title))
               )
             (setq ast (org-noter--parse-root))
             (org-noter--narrow-to-root ast)
             (goto-char (org-element-property :begin ast))
             )
           )
         )
       )
      (t (user-error "This command is only supported on PDF Tools or Nov.")))
     )))

(defun org-noter-plus-insert-annots-by-pdf-tool (pdf-file)
  (let* ((output-data (org-noter-plus--pdf-skeletion-info pdf-file)))
               (dolist (item output-data)
               (let* ((type  (alist-get 'type item))
                      (page  (alist-get 'page item))
                      (level 0))
                 ;; outline
                 (when (and (eq type 'goto-dest) (> page 0))
                   (let ((depth (alist-get 'depth item))
                         (title (alist-get 'title item))
                         (top   (alist-get 'top item))
                         )
                     (insert (format
                              "\n%s- [[%s:%s::%s][%s]]"
                              (make-string depth ? )
                              org-noter-plus-pdf-link-prefix
                              pdf-file
                              page
                              title))
                     (setq level depth)
                     ))
                 ;; annots
                 (when (not (eq type 'goto-dest))
                   (let* ((type-name (cond
                                      ;; TODO let user customize this?
                                      ((eq type 'highlight)  "Highlight")
                                      ((eq type 'underline)  "Underline")
                                      ((eq type 'squiggly)   "Squiggly")
                                      ((eq type 'text)       "Text note")
                                      ((eq type 'square)       "Square")
                                      ((eq type 'strike-out) "Strikeout")))
                          (height (nth 1 (assoc-default 'edges item)))
                          (id (symbol-name (assoc-default 'id item)))
                          (contents (assoc-default 'contents item))
                          )
                     (insert (format
                              "\n%s- %s [[%s:%s::%s++%s][%s]] %s"
                              (make-string (+ level 1) ? )
                              type-name
                              org-noter-plus-pdf-link-prefix
                              pdf-file
                              page
                              height
                              id
                              (car contents)
                              ))
                     ;; square
                     (when (eq type 'square)
                       (let* ((imagefile (concat org-noter-plus-image-dir
                                                 (file-name-base pdf-file)
                                                 "-" id ".png")))
                         (insert (format
                                  " [[file:%s]]"
                                  imagefile
                                  ))
                         ))
                     ;; other note: put text in quote box,
                     ;; in order not to break the list structure
                     (when (and
                            (memq type
                                  '(highlight underline squiggly text strike-out))
                            (cdr contents))
                       (org-return-indent)
                       (org-insert-structure-template "quote")
                       (beginning-of-line)
                       (insert (format
                                "%s\n"
                                (cdr contents)
                                ))
                       (end-of-line)
                       )))
                 ))

         )

  )


;;;;; import & export pdf toc
;; TODO match the pdfhelper version
(defvar org-noter-plus-toc-path (expand-file-name "toc.org" temporary-file-directory))

(defvar org-noter-plus--pdf-dealing-with nil)

(defun org-noter-plus-export-pdf-toc ()
  (interactive)
  (when (derived-mode-p 'pdf-view-mode)
    (setq org-noter-plus--pdf-dealing-with pdf-view--server-file-name)
    (let ((cmd (format "python3 '%s' '%s' -te --toc-path '%s'" org-noter-plus--toc-script
                       pdf-view--server-file-name org-noter-plus-toc-path)))
      (call-process-shell-command cmd)
      (find-file org-noter-plus-toc-path))))

(defun org-noter-plus-import-pdf-toc ()
  (interactive)
  (let ((cmd (format "python3 '%s' '%s' -ti --toc-path '%s'" org-noter-plus--toc-script
                     org-noter-plus--pdf-dealing-with org-noter-plus-toc-path)))
    (call-process-shell-command cmd)
    (setq org-noter-plus--pdf-dealing-with nil)))

;;;; Footer
(provide 'org-noter-plus)
;;; org-noter-plus.el ends here
