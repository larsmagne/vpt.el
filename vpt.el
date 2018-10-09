;;; vpt.el --- Variable Pitch Tables -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: tables

;; vpt is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; vpt is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar variable-pitch-table-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'variable-pitch-table-sort-by-column)
    map))

(defun variable-pitch-table (heading data)
  "Display a table using a variable pitch font."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (loop for i from 0
	    for spec in heading
	    do (goto-char (point-min))
	    (vpt--column i spec data)))))

(defun vpt--limit-string (string length)
  (if (or (not length)
	  (< (length string) length))
      string
    (substring string 0 length)))

(defun vpt--column (i spec lines &optional data)
  (let ((width (getf spec :width))
	(name (getf spec :name))
	(max 0)
	header-item-start)
    (end-of-line)
    (setq header-item-start (point))
    (insert (vpt--limit-string (propertize name 'vpt-index i) width))
    (when (zerop i)
      (insert "\n"))
    ;; Insert the column.
    (loop for line in lines
	  for elem = (elt line i)
	  do (forward-line 1)
	  (end-of-line)
	  (insert (propertize (vpt--limit-string elem width)
			      'vpt-value elem))
	  (when (zerop i)
	    (insert "\n")))
    ;; Figure out the max width.
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (setq max (max max (car (window-text-pixel-size
			       nil (line-beginning-position) (point)))))
      (forward-line 1))
    ;; Indent to the max width (but not on the final item on the line).
    (unless (= i (1- (length (car lines))))
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(insert (propertize " " 'display `(space :align-to (,(+ max 20)))))
	(forward-line 1)))
    ;; Insert the data.
    (goto-char (point-min))
    (forward-line 1)
    (dolist (elem lines)
      (put-text-property (point) (line-end-position) 'vpt-data elem)
      (forward-line 1))
    ;; And metadata.
    (goto-char (point-min))
    (forward-line 1)
    (dolist (elem data)
      (put-text-property (point) (line-end-position) 'data elem)
      (forward-line 1))
    ;; Fix up the header line.
    (goto-char (point-min))
    (end-of-line)
    (insert (propertize " " 'display `(space :align-to (,(+ max 20)))))
    (add-text-properties header-item-start (point)
			 `(face (variable-pitch :background "#808080")
				local-map ,variable-pitch-table-map))))

(defun variable-pitch-table-sort-by-column (&optional reverse)
  "Sort the table by the column under point."
  (interactive "P")
  (let ((column (get-text-property (point) 'vpt-index)))
    (save-excursion
      (save-restriction
	(narrow-to-region
	 (progn
	   (forward-line 1)
	   (point))
	 (loop while (get-text-property (point) 'vpt-data)
	       do (forward-line 1)
	       finally (return (point))))
	(goto-char (point-min))
	(sort-subr
	 reverse
	 (lambda ()
	   (forward-line 1))
	 'end-of-line
	 nil
	 nil
	 (lambda (a1 a2)
	   (string< (elt (get-text-property (car a1) 'vpt-data) column)
		    (elt (get-text-property (car a2) 'vpt-data) column))))))))

(provide 'vpt)

;;; vpt.el ends here
