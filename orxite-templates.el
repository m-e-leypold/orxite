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

(require 'orxite-utils)
(require 'orxite-page-db)

;;; * A minimal template ------------------------------------------------------

(defun orxite-minimal-template (content)
  (orxite-outer-template
   `(div/content.content
     ,content)))

;;; * Template building blocks ------------------------------------------------

;; TODO: with-orxite-building-block

(defun @:a-to-path (text path)   ;; TODO: alias: orxite-link-to-path
  `(:a
   (@
    :href ,(concat (@page :relative-root) path))
   ,text))

(defun orxite-siteMenu (&rest items)
  (if (equal items '(:config))
      (setf items (orxite-site-attribute :siteMenu)))
  (if (not items)
      '()
    (let ((links '()))
      (while items
	(let ((text (car items))
	      (path (cadr items)))
	  (push `(:a (@ :href ,(concat (@page :relative-root) path)) ,text)
		links)
	  (setf items (cddr items))))
      `(:div/siteMenu.siteMenu
	,(orxite-join-list links " ")))))

(defun orxite-siteTitle ()
  `(:h1/siteTitle.siteTitle
    (:a
     (@ :href ,(concat (@page :relative-root) "index.html"))
     ,(or (@site :banner) (@site :title)))))

(defun orxite-siteSubtitle ()
  `(:div/siteSubtitle.siteSubtitle
    ,(or (@site :sub-title) "")))

(defun orxite-printOrigin ()
  `(:div/printOrigin.printOrigin
    (:a
     (@
      :href ,(@page :absolute-url))
     ,(@page :absolute-url))))
  
(defun orxite-absolute-link-to-page ()
  ;; TODO: Optionally override link text  
  `(:a
    (@
     :href ,(@page :absolute-url))
    ,(@page :absolute-url)))


(defun orxite-permalink-to-page ()
  `(:a
    (@
     :href ,(@page :permalink-url))
    ,(@page :permalink-path)))

(defun orxite-simple-header ()
  `(:div/header.header
    ,(orxite-siteTitle)
    ,(orxite-siteSubtitle)
    ,(orxite-siteMenu :config)
    ,(orxite-printOrigin)))

(defun orxite-tombstone+links-footer ()
    `(:div/footer.footer
      (:span/tombstone.tombstone
       "⯀")    
      " © "
      ,(@site :owner)
      " ○ "
      ,(orxite-absolute-link-to-page)
      ,@(if (@page :id)
	    `(" ○ "
	      ,(orxite-permalink-to-page)))))
   
(defvar orxite-footers
  '( :tombstone+links orxite-tombstone+links-footer))

(defun @footer (key &rest args)
  (apply (plist-get orxite-footers key) args))

(defvar orxite-headers
  '( :simple orxite-simple-header))

(defun @header (key &rest args)
  (apply (plist-get orxite-headers key) args))

;;; * The default template ----------------------------------------------------

(defun orxite-default-template (content)
  (orxite-outer-template
   `( ,(@header :simple)     
      (:div/content.content
       ,content
       )
      ,(@footer :tombstone+links))))

;;; * #

(provide 'orxite-templates)
