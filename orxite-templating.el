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
;;;

;;; * Requires ----------------------------------------------------------------

(require 'org-capture)
(require 'jack)

;;; * HTML generation ---------------------------------------------------------

(defun orxite-maybe-to-html (tree-or-string)
  (if (stringp tree-or-string)
      tree-or-string
    (jack-html tree-or-string)))

;;; * Outer templates ---------------------------------------------------------
;;;
;;;   Outer templates are just strings stored in files

(defvar orxite-outer-template nil)
(defvar orxite-inner-content  nil)  ;; used during outer template expansion

(defun orxite-inner-content ()
  orxite-inner-content)

(defun orxite-expand-outer-template (content template)
  (let ((orxite-inner-content content))
    (with-temp-buffer
      (insert template)
      (org-capture-expand-embedded-elisp 'mark)
      (org-capture-expand-embedded-elisp)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun orxite-outer-template (content)
  (orxite-expand-outer-template (orxite-maybe-to-html content) orxite-outer-template))

(defun orxite-read-outer-template (file) ;; into currrent buffer
  (with-temp-buffer
    (insert-file file)
    (buffer-substring-no-properties (point-min) (point-max))))

;;; * Applying templates ------------------------------------------------------

(defvar orxite-template nil) ;; maybe we don't need this. This is per page.

(defun orxite-apply-templates (content &optional inner-template outer-template)
  (message "Inner template: %s" inner-template)
  (message "Outer template: %s" outer-template)
  (let ((orxite-outer-template
	 (orxite-read-outer-template outer-template)))
    (orxite-maybe-to-html (funcall inner-template content))))
  
(provide 'orxite-templating)
