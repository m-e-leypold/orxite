;;; *                                                 -*- lexical-binding: t -*-
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

;;; * -------------------------------------------------------------------------

(defun orxite-read-file (file)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (setf page-data (read (current-buffer)))
    page-data))

(defun orxite-string-to-boolean (strings)
  (if (not strings)
      nil
    (progn
      (let ((value strings))
	(if (consp value)
	    (setf value (car value)))
	(or
	 (equal value "t")
	 (equal value "true")
	 (equal value "yes"))))))

(defun orxite-merge-lines (strings)
  (if (not strings)
      nil
    (if (stringp strings)
	strings
      (string-join strings " "))))

(defun orxite-merge-word-lists (strings)
  (if (consp strings)
      (setf strings (orxite-merge-lines strings)))
  (split-string strings " " t))
  
(defun orxite-plist-remove-key (plist key-to-remove)
  (let ((collected '()))
    (while plist
      (let ((key (car plist))
	    (value (cadr plist)))
	(if (not (equal key key-to-remove))
	    (progn
	      (push key collected)
	      (push value collected))))
      (setf plist (cddr plist)))
    (reverse collected)))            ;; TODO could be more efficient.

(defun orxite-map-array (fun a)  
  (let ((collected '())
	(n-items (length a))
	(i 0))
    (while (< i n-items)
      (push (funcall fun (elt a i)) collected)
      (setf i (1+ i))
      (reverse collected))))


(defun orxite-related-file-name (path extension)
  (concat (file-name-sans-extension path) "." extension))

(defun orxite-write-to-file (data path)
  (with-temp-buffer
    (insert (format "%S" data))
    (write-file path)))

(defun orxite-plist-apply-defaults (plist defaults)
  (let ((rest defaults)
	(result plist))
    (while rest
      (let ((key (car rest))
            (value (cadr rest)))

	(if (not (plist-get plist key))
	    (setf result (setf (plist-get plist key) value)))
	
        (setf rest (cddr rest))))
    result))


(defun orxite-plist-to-rec (plist)
  (let ((rest plist))
    (while rest
      (let ((key (car rest))
            (value (cadr rest)))
        (setf rest (cddr rest))
	(let ((field-key
	       (format "%s"
		       (subst-char-in-string
			?- ?_
			(substring (symbol-name key) 1))))
	      (values (if (consp value) value (list value))))
	  (dolist (v values)
            (insert field-key ": " (format "%s" v) "\n")))))))

(defun orxite-write-rec (plist file)
  (with-temp-buffer
    (orxite-plist-to-rec plist)
    (write-file file)))

;; Should there be a way to have a preferred order of writing plists?

(defun orxite-join-list (items separator)
  (if (not items)
      '()    
    (let ((result '())
	  (first  (car items))
	  (items  (cdr items)))
      (push first result)
      (while items	
	(push " " result)
	(push (car items) result)
	(setf items (cdr items)))
      result)))

(provide 'orxite-utils)
