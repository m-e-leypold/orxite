;;;                                                   -*- lexical-binding: t -*-
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

;; * Environment --------------------------------------------------------------

(require 'orxite-utils)

;; * Build Config --------------------------------------------------------------

(defvar orxite-build-setup nil)

(defun orxite-site-config-file ()
  (plist-get orxite-build-setup :site-config))

(defun orxite-scripts-path ()
  (plist-get orxite-build-setup :scripts))

(defun orxite-page-db-file ()
  (plist-get orxite-build-setup :page-db))

(defun orxite-load-build-setup (build-setup)
  (setf orxite-build-setup (orxite-read-file build-setup))
  (message "Build setup: %S" orxite-build-setup))


;; * Site Config --------------------------------------------------------------

(defvar orxite-site-config nil)

(defun orxite-site-config (config)
  (setf orxite-site-config config))

(defun blog-site-config (config)       ;; TODO: Remove. Temp compat measure
  (orxite-site-config config))

(defun orxite-site-attribute (key)
  (plist-get orxite-site-config key))

(defun orxite-load-site-config (config-file)
  (load-file config-file)
  (message "Site setup: %S" orxite-site-config))

;; * Page DB ------------------------------------------------------------------

(defvar orxite-page-db nil)

(defun orxite-page-attributes (page-path)
  ;; TODO: Paths should be normalized       
  (cl-find page-path orxite-page-db
	   :test #'equal
	   :key #'(lambda (x) (plist-get x :page-path))))


(defun orxite-load-page-db (file)
  (setf orxite-page-db (orxite-read-file file))
  (message "\nPage DB: %S\n" orxite-page-db))

(defun orxite-load-page-metadata-as-page-db (file)
  (setf orxite-page-db (list (orxite-read-file file)))
  (message "\nPage DB: %S\n" orxite-page-db))

;; * Page attributes ----------------------------------------------------------

(defvar orxite-page-attributes nil)

(defun orxite-page-inner-template ()
  'orxite-default-template)

(defun orxite-page-outer-template ()
  (concat (orxite-scripts-path) "/default-template.html"))

(defun orxite-page-attribute (key)
  (plist-get orxite-page-attributes key))

(defun orxite-page-relative-root ()
  (apply #'concat
	 (mapcar #'(lambda (x) "../")
		 (cdr (split-string
		       (orxite-page-attribute :page-path) "/" t)))))

(defun orxite-page-absolute-url ()
  (concat "https://" (orxite-site-attribute :url)
	  "/" (orxite-page-attribute :page-path) ".html"))

(defun orxite-page-permalink-url ()
  (concat "https://" (orxite-site-attribute :url)
	  "/perma/" (orxite-page-attribute :id) ".html"))

(defun orxite-page-permalink-path ()
  (concat "/perma/" (orxite-page-attribute :id) ".html"))


(defun orxite-configure-page-attributes (page-path)
  (if (and orxite-site-config orxite-page-db)
      (progn
	(setf orxite-page-attributes (orxite-page-attributes page-path))
	(message "Page Attributes: %S" orxite-page-attributes))))

;; * Page DB ------------------------------------------------------------------

(defun orxite-page-db-load-setup
    (build-setup &optional page-path page-metadata)
  
  ;; page-db might be the metadata file of a single page
  
  (orxite-load-build-setup build-setup)
  (orxite-load-site-config (orxite-site-config-file))
  
  (if page-metadata
      (orxite-load-page-metadata-as-page-db page-metadata)
    (orxite-load-page-db (orxite-page-db-file)))

  ;; TODO should do some amount of page-path guessing here, using the staging-path

  (if page-path
      (orxite-configure-page-attributes page-path)))


;; * Simplified vocabulary (@-syntax) ----------------------------------------

(defvar orxite-page-virtual-attributes
  '(
    :relative-root orxite-page-relative-root
    :absolute-url  orxite-page-absolute-url
    :permalink-url orxite-page-permalink-url
    :permalink-path orxite-page-permalink-path
    ))

(defun @page (key)
    ;; TODO: extend this to a processing pipeline

  (let ((virtual-attribute
	 (plist-get orxite-page-virtual-attributes key)))
    (if virtual-attribute
	(funcall virtual-attribute)
	(orxite-page-attribute key))))


(defun @site (key)
  ;; TODO: extend this to a processing pipeline
  (orxite-site-attribute key))


(provide 'orxite-page-db)

