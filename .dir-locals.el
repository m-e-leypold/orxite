;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((python-mode . ((outline-heading-end-regexp . "\n")
		 (fill-column . 72)
		 (column-enforce-column . 79)
		 (eval . (progn
                           (outshine-mode 1)
                           (column-enforce-mode 1)
                           (toggle-truncate-lines 1)
			   (outline-minor-mode)))
		 ))
 (emacs-lisp-mode . ((fill-column . 72)
		     (column-enforce-column . 79)
		     (eval . (progn
			       (outshine-mode)
			       (toggle-truncate-lines 1)
			       (outline-minor-mode)
			       (column-enforce-mode))))))

