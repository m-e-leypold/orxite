;;;                                                  -*- lexical-binding: t -*-
;;;
;;; Orxite - A static site generator for org format source
;;; Copyright (C) 2023  M E Leypold
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

(require 'ox)
(require 'orxite-page-db)
(require 'orxite-templating)
(require 'orxite-templates)

(defconst orxite-html-export-settings
  '( :section-numbers nil
     :with-toc nil))

(defun orxite-cleanup-generated-html (html)
  (with-temp-buffer
    (insert html)

    ;; now, removing the outer div (heuristically)

    (goto-char (point-min))
    (next-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (previous-line)
    (beginning-of-line)
    (left-char)
    (delete-region (point) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun orxite-export-to-html
    (source-file &optional target-file build-setup page-path metadata-source)

  (message "Translating: %s" source-file)
  (message "Page path  : %s" page-path)
  (message "Destination: %s" (if target-file target-file "[default]"))
  (message "Build setup: %s" build-setup)
  
  (message "Metadata source: %s"
	   (if metadata-source metadata-source "[page-db]"))

  (setf source-file (expand-file-name source-file))

  (if build-setup
      (orxite-page-db-load-setup build-setup page-path metadata-source))

  (with-current-buffer (find-file-noselect source-file)
    (let* ((html (orxite-cleanup-generated-html
		  (org-export-as 'html nil nil t orxite-html-export-settings))))

      (if (not target-file)
	  (setf target-file
		(expand-file-name (org-export-output-file-name ".html" nil))))
      (message "Destination: %s" target-file)

      (with-current-buffer (find-file-noselect target-file)
	(widen)
	(delete-region (point-min) (point-max))
	(goto-char (point-min))
	(insert (orxite-apply-templates
		 html
		 (orxite-page-inner-template)
		 (orxite-page-outer-template)))
	(save-buffer)))))

(provide 'orxite-export)
