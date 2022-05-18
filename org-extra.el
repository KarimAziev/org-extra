;;; org-extra.el --- Configure extra -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-extra
;; Keywords: outlines
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This file configures operations with extra

;; Commands

;; M-x `org-extra-smart-beginning-of-line'
;;      Move point to first non-whitespace character or beginning-of-line.

;; M-x `org-extra-back-to-heading'
;;      Move to the heading line of which the present line is a subheading.

;; Advice functions
       `org-extra-src-fontify-advice'
       `org-extra-babel-before-execute-src-block'
;;; Code:


(require 'org)
(require 'straight)

(defvar org-src-block-faces)

(declare-function org-src-get-lang-mode "org-src")
(declare-function org-up-heading-safe "org")

(defface org-extra-checkbox-done-text
  '((t (:foreground "#5a637b")))
  "Face for the text part of a checked `org-mode' checkbox."
  :group 'org)

(defun org-extra-extend-faces ()
  "Add extra face to `org-mode'."
  (font-lock-add-keywords
   'org-mode
   '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-extra-checkbox-done-text prepend))
   'append))

(defvar org-extra-straight-dir-mod-time nil
  "Last modification time of straight repos directory.")

(defvar org-extra-ob-packages-cached nil
  "List of cached file name bases with trimmed ob-prefix.")

(defun org-extra-ob-packages ()
	"Search for files prefixed with `ob-' in straight repos directory.
The returned value is a list of file name bases with trimmed ob-prefix, e.g.
python for ob-python, julia from ob-julia and so on.
Result is cached and stored in `org-extra-ob-packages-cached', and invalidated
by `org-extra-straight-dir-mod-time' - modification time of straight repos
 directory."
  (let ((mod-time (file-attribute-modification-time (file-attributes
                                                     (straight--repos-dir)))))
    (unless (equal mod-time org-extra-straight-dir-mod-time)
      (setq org-extra-ob-packages-cached
            (mapcar
             (lambda (it)
               (replace-regexp-in-string "^ob-" ""
                                         (file-name-base it)))
             (directory-files-recursively (straight--repos-dir) "^ob-.*el$")))
      (setq org-extra-straight-dir-mod-time mod-time))
    org-extra-ob-packages-cached))

(defvar org-src-lang-modes)
(defvar org-babel-load-languages)

(defun org-extra-babel-before-execute-src-block (&optional _arg info _params)
  "Advice function for `org-babel-execute-src-block' to load language from INFO.
Usage:
\(advice-add 'org-babel-execute-src-block :before #'km-org-babel-before-execute-src-block)."
  (when-let* ((lang (nth 0 info))
              (sym (intern lang)))
    (unless (assq sym org-babel-load-languages)
      (let ((cands (delete-dups (append
                                 (list sym)
                                 (mapcar (lambda (c) (intern (car c)))
                                         (seq-filter (lambda (it) (eq sym (cdr it)))
                                                     org-src-lang-modes)))))
            (found))
        (setq found (seq-find (lambda (l)
                                (require
                                 (intern (format "ob-%s" l)) nil t))
                              cands))
        (when found
          (add-to-list 'org-babel-load-languages `(,found . t))
          (org-babel-do-load-languages
           'org-babel-load-languages
           org-babel-load-languages))))))

;;;###autoload
(defun org-extra-back-to-heading ()
  "Move to the heading line of which the present line is a subheading."
  (interactive)
  (org-up-heading-safe))

;;;###autoload
(defun org-extra-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (if (save-excursion (forward-char -1)
                      (invisible-p (point)))
      (progn (forward-char -1)
             (while (invisible-p (point))
               (forward-char -1))
             (beginning-of-line))
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line)))))

(defun org-extra-src-fontify-advice (lang start end)
  "Fontify code block between START and END using LANG's syntax.
The difference between this function and `org-src-font-lock-fontify-block'
is that this function delay language mode hooks to increase speedup.

Usage:

\(advice-add #'org-src-font-lock-fontify-block
  :override #'org-extra-src-fontify-advice)."
  (let ((lang-mode (org-src-get-lang-mode lang)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
	          (modified (buffer-modified-p))
	          (org-buffer (current-buffer)))
	      (remove-text-properties start end '(face nil))
	      (with-current-buffer
	          (get-buffer-create
	           (format " *org-src-fontification:%s*" lang-mode))
	        (let ((inhibit-modification-hooks nil))
	          (erase-buffer)
	          ;; Add string and a final space to ensure property change.
	          (insert string " "))
          (delay-mode-hooks
            (unless (eq major-mode lang-mode) (funcall lang-mode))
	          (font-lock-ensure)
	          (let ((pos (point-min)) next)
	            (while (setq next (next-property-change pos))
	              ;; Handle additional properties from font-lock, so as to
	              ;; preserve, e.g., composition.
	              (dolist (prop (cons 'face font-lock-extra-managed-props))
		              (let ((new-prop (get-text-property pos prop)))
		                (put-text-property
		                 (+ start (1- pos)) (1- (+ start next)) prop new-prop
		                 org-buffer)))
	              (setq pos next)))
            (set-buffer-modified-p nil)))
	      ;; Add Org faces.
	      (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
          (when (or (facep src-face) (listp src-face))
            (font-lock-append-text-property start end 'face src-face))
	        (font-lock-append-text-property start end 'face 'org-block))
	      (add-text-properties
	       start end
	       '(font-lock-fontified t fontified t font-lock-multiline t))
	      (set-buffer-modified-p modified)))))

(provide 'org-extra)
;;; org-extra.el ends here