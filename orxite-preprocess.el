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

;;; * Requires ----------------------------------------------------------------

(require 'orxite-page-db)

;;; * Metadata extraction -----------------------------------------------------

;; (defconst blog-keywords-to-collect
;;   '(("TITLE"   :title   . blog-merge-strings)
;;     ("TEASER"  :teaser  . blog-merge-strings)   
;;     ("FORMAT"  :format  . car)
;;     ("DATE"    :date    . car)
;;     ("PUBLISH" :publish . car)
;;     ("REQUIRES-PAGE-DB" :requires-page-db . blog-string-to-boolean)
;;     ("DUMMY"   :dummy  . identity)))

(defconst orxite-keywords-to-collect
  '((:title     "TITLE"  orxite-merge-lines)
    (:teaser    "TEASER" orxite-merge-lines)
    (:tags      "TAGS"   orxite-merge-word-lists)       
    (:format    "FORMAT" car)
    (:page-path "PAGE_PATH" car)
    (:date      "DATE"   car)
    (:requires-page-db "REQUIRES-PAGE-DB" orxite-string-to-boolean)))

;; TODO: Add tags here => orxite-merge-word-lists

(defun orxite-keyword-list-to-plist (mapping keyword-list)
  (let ((result '()))
    (dolist (key-value keyword-list (reverse result))
      (let ((keyword (car key-value))
	    (value   (cdr key-value)))		     
	(let ((item (cl-find keyword mapping :test #'equal :key #'cadr)))
	  (push (car item) result)
	  (push (funcall (caddr item) value) result)
	  )))))

(defun orxite-metadata-from-parsed (parsed)
  (let ((headline (elt parsed 2)))
    (cl-assert (equal (car headline) 'headline))
    (let ((props (elt headline 1)))
      `( :title  ,(car (plist-get props :title))
	 :id     ,(plist-get props :ID)))))

(defun orxite-collect-metadata (parsed)

  (let ((metadata-from-parsed (orxite-metadata-from-parsed parsed)))
  
    (let ((keywords-to-collect
	   (mapcar #'(lambda (x) (cadr x)) 
		   orxite-keywords-to-collect)))
      (message "keywords: %S" keywords-to-collect)    
      (let ((metadata-from-keywords
	     (orxite-keyword-list-to-plist
	      orxite-keywords-to-collect
	      (org-collect-keywords keywords-to-collect))))
	(message "metadata-from-keywords: %S" metadata-from-keywords)
	(orxite-plist-apply-defaults
	 metadata-from-keywords metadata-from-parsed)))))

;;; * Parsing -----------------------------------------------------------------


(defun orxite-clean-parsed-org (object)
  (if (not (consp object))
      (progn 
	(if (stringp object)
	    (substring-no-properties object)
	  (if (vectorp object)
	      (orxite-map-array #'orxite-clean-parsed-org object)
	    object)))
    (progn
      (if (and object (keywordp (car object)))
	  (setf object (orxite-plist-remove-key object :parent)))
      (let ((mapped 
	     (mapcar #'orxite-clean-parsed-org object)))
	mapped))))

(defun orxite-parse-buffer (buffer)
  (message "Parsing: %S" buffer)
  (with-current-buffer buffer
    (widen)
    (orxite-clean-parsed-org (org-element-parse-buffer))))

;;; * Preprocessing -----------------------------------------------------------

(defun orxite-maybe-insert-metadata-section ()
  (if (not (search-forward-regexp "^[*] +\\#" nil t))
      (progn
	(goto-char (point-max))
	(insert "\n* #   :ARCHIVE:noexport:"
      ))))
  
(defun orxite-insert-page-path (path)
  (delete-matching-lines "^#+PAGE_PATH:")
  (goto-char (point-max))
  (insert (format "\n#+PAGE_PATH: %s" path)))
	     
(defun orxite-insert-setup ()
  (goto-char (point-max))
  (insert (format "\n#+SETUPFILE: %s/macros.org" (orxite-scripts-path))))

(defun orxite-preprocess
    (source-file &optional target-file build-setup page-path)
    (message "Preprocessing: %s" source-file)
    (message "Page path    : %s" page-path)
    (message "Destination  : %s" (if target-file target-file "[default]"))
    (message "Build setup  : %s" build-setup)

    (if build-setup
	(progn 
	  (orxite-load-build-setup build-setup)
	  (orxite-load-site-config (orxite-site-config-file))))

    ;; TODO: Guess page path if not given (strip one subdir under
    ;; .build)
    
    (setf source-file (expand-file-name source-file))
    (setf target-file (expand-file-name target-file))

    (let ((target-file-meta   (orxite-related-file-name target-file "meta"))
	  (target-file-rec    (orxite-related-file-name target-file "rec"))
	  (target-file-parsed (orxite-related-file-name target-file "parsed")))
	  
    
    ;; TODO: More derived path names
    
      (with-temp-buffer
	(insert-file source-file)
	(orxite-maybe-insert-metadata-section)
	(if page-path
	    (orxite-insert-page-path page-path))
	(if build-setup
	    (orxite-insert-setup))
	(org-mode)

	;; TODO: Remove the bloody header tags
	
	(let ((parsed (orxite-parse-buffer (current-buffer))))
	  (let ((metadata (orxite-collect-metadata parsed))) 
	  
	    ;; TODO write rec
	    
	    (orxite-write-to-file metadata target-file-meta)
	    (orxite-write-rec metadata target-file-rec)
	    (orxite-write-to-file parsed target-file-parsed)
	    	  
	    ;; page-db deps are better handled from the makefile
	    (write-file target-file))))))

(provide 'orxite-preprocess)
