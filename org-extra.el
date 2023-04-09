;;; org-extra.el --- Configure extra -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-extra
;; Keywords: outlines
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (org "9.6") (transient "0.3.7.50"))

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
     ;; `org-extra-src-fontify-advice'
     ;; `org-extra-babel-before-execute-src-block'
;;; Code:

(require 'org)
(require 'straight)
(require 'transient)
(defvar org-src-block-faces)
(defconst org-extra-preview-data-root
  (file-name-directory (if (bound-and-true-p load-file-name) load-file-name
                         (buffer-file-name)))
  "The directory where `org-extra' package exists.")

(declare-function org-src-get-lang-mode "org-src")
(declare-function org-up-heading-safe "org")
(defface org-extra-checkbox-done-text
  '((t (:foreground "#5a637b")))
  "Face for the text part of a checked `org-mode' checkbox."
  :group 'org)

(defcustom org-extra-languages-alist '((c . "C")
                                       (emacslisp . "emacs-lisp")
                                       (javascript . "js"))
  "Alist of languages from `language-detection.el' and org babel languages."
  :group 'org-babel
  :type '(alist
          :tag "Languages"
          :key-type
          (choice
           (const :tag "Ada" ada)
           (const :tag "Awk" awk)
           (const :tag "C" c)
           (const :tag "Clojure" clojure)
           (const :tag "Cpp" cpp)
           (const :tag "Csharp" csharp)
           (const :tag "Css" css)
           (const :tag "Dart" dart)
           (const :tag "Delphi" delphi)
           (const :tag "Emacslisp" emacslisp)
           (const :tag "Erlang" erlang)
           (const :tag "Fortran" fortran)
           (const :tag "Fsharp" fsharp)
           (const :tag "Go" go)
           (const :tag "Groovy" groovy)
           (const :tag "Haskell" haskell)
           (const :tag "Html" html)
           (const :tag "Java" java)
           (const :tag "Javascript" javascript)
           (const :tag "Json" json)
           (const :tag "Latex" latex)
           (const :tag "Lisp" lisp)
           (const :tag "Lua" lua)
           (const :tag "Matlab" matlab)
           (const :tag "Objc" objc)
           (const :tag "Perl" perl)
           (const :tag "Php" php)
           (const :tag "Prolog" prolog)
           (const :tag "Python" python)
           (const :tag "R" r)
           (const :tag "Ruby" ruby)
           (const :tag "Rust" rust)
           (const :tag "Scala" scala)
           (const :tag "Shell" shell)
           (const :tag "Smalltalk" smalltalk)
           (const :tag "Sql" sql)
           (const :tag "Swift" swift)
           (const :tag "Visualbasic" visualbasic)
           (const :tag "Xml" xml)
           (symbol :tag "Other"))
          :value-type (string :tag "Org babel language")))

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

(defvar org-src-lang-modes)
(defvar org-babel-load-languages)
(defun org-extra-babel-default-loadable-languages ()
  "Return list of options from variable `org-babel-load-languages'."
  (mapcar (lambda (it)
            (car (last it)))
          (cdr (plist-get (cdr (get
                                'org-babel-load-languages
                                'custom-type))
                          :key-type))))

(defun org-extra-read-babel-languages (prompt &optional predicate require-match
                                              initial-input hist def
                                              inherit-input-method)
  "Read org babel language with PROMPT.
Optional arguments PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF and
INHERIT-INPUT-METHOD have the same meaning as for `completing-read'."
  (completing-read prompt (delete-dups
                           (append
                            (mapcar #'car org-babel-load-languages)
                            (mapcar (lambda (it)
                                      (car (reverse it)))
                                    (cdr (nth 1
                                              (memq :key-type
                                                    (get
                                                     'org-babel-load-languages
                                                     'custom-type)))))))
                   predicate require-match
                   initial-input hist def
                   inherit-input-method))

(defun org-extra-ob-packages ()
  "Search for files prefixed with `ob-' in straight repos directory.
The returned value is a list of file name bases with trimmed ob-prefix, e.g.
python for ob-python, julia from ob-julia and so on.
Result is cached and stored in `org-extra-ob-packages-cached', and invalidated
by `org-extra-straight-dir-mod-time' - modification time of straight repos
 directory."
  (let ((mod-time (file-attribute-modification-time
                   (file-attributes
                    (straight--repos-dir)))))
    (unless (equal mod-time
                   org-extra-straight-dir-mod-time)
      (setq
       org-extra-ob-packages-cached
       (mapcar
        (lambda (it)
          (replace-regexp-in-string
           "^ob-" ""
           (file-name-base it)))
        (directory-files-recursively
         (straight--repos-dir)
         "^ob-.*el$")))
      (setq org-extra-straight-dir-mod-time mod-time))
    org-extra-ob-packages-cached))

(defun org-extra-babel-load-language (lang)
  "Add LANG to `org-babel-load-languages'."
  (let ((sym (if (stringp lang)
                 (intern lang) lang)))
    (unless (assq sym org-babel-load-languages)
      (let ((cands (delete-dups (append
                                 (list sym)
                                 (mapcar (lambda (c)
                                           (intern (car c)))
                                         (seq-filter (lambda (it)
                                                       (eq sym (cdr it)))
                                                     org-src-lang-modes)))))
            (found))
        (setq found (seq-find (lambda (l)
                                (require (intern (format "ob-%s" l)) nil t))
                              cands))
        (when found
          (add-to-list 'org-babel-load-languages `(,found . t))
          (set-default 'org-babel-load-languages org-babel-load-languages)
          (require (intern (concat "ob-" (symbol-name found)))))))))

(defun org-extra-babel-before-execute-src-block (&optional _arg info _params)
  "Advice function for `org-babel-execute-src-block' to load language from INFO.

Usage:


\\=(advice-add \\='org-babel-execute-src-block
              :before #\\='org-extra-babel-before-execute-src-block)."
  (org-extra-babel-load-language (nth 0 info)))

(defun org-extra-load-languages-in-buffer ()
  "Try to load all src-block languages in current buffer."
  (let ((langs)
        (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (when (org-babel-active-location-p)
          (goto-char (match-beginning 0))
          (let ((end-block (match-end 0))
                (lang (match-string 2)))
            (when lang
              (unless (member lang langs)
                (push lang langs)
                (org-extra-babel-load-language lang)))
            (goto-char end-block)))))))

;;;###autoload
(defun org-extra-back-to-heading ()
  "Move to the heading line of which the present line is a subheading."
  (interactive)
  (org-up-heading-safe))

;;;###autoload
(defun org-extra-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (if (save-excursion
        (forward-char -1)
        (invisible-p (point)))
      (progn (forward-char -1)
             (while (invisible-p (point))
               (forward-char -1))
             (when (re-search-backward "\n" nil t 1)
               (forward-char 1))
             (back-to-indentation))
    (progn
      (let ((old-pos (point)))
        (back-to-indentation)
        (and (= old-pos (point))
             (if (re-search-backward "\n" nil t 1)
                 (forward-char 1)
               (beginning-of-line)))))))

(defun org-extra-src-fontify-advice (lang start end)
  "Fontify code block between START and END using LANG's syntax.
The difference between this function and `org-src-font-lock-fontify-block'
is that this function delay language mode hooks to increase speedup.

Usage:

\=(advice-add #\='org-src-font-lock-fontify-block
              :override #\='org-extra-src-fontify-advice)."
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
            (unless (eq major-mode lang-mode)
              (funcall lang-mode))
            (font-lock-ensure)
            (let ((pos (point-min)) next)
              (while (setq next (next-property-change pos))
                ;; Handle additional properties from font-lock, so as to
                ;; preserve, e.g., composition.
                (dolist (prop (cons 'face font-lock-extra-managed-props))
                  (let ((new-prop (get-text-property pos prop)))
                    (put-text-property
                     (+ start (1- pos))
                     (1- (+ start next)) prop new-prop
                     org-buffer)))
                (setq pos next)))
            (set-buffer-modified-p nil)))
        ;; Add Org faces.
        (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
          (when (or (facep src-face)
                    (listp src-face))
            (font-lock-append-text-property start end 'face src-face))
          (font-lock-append-text-property start end 'face 'org-block))
        (add-text-properties
         start end
         '(font-lock-fontified t fontified t font-lock-multiline t))
        (set-buffer-modified-p modified)))))

(defun org-extra-bounds-of-current-block ()
  "Return list of (BLOCK-TYPE BEGINNING END).
Beginning and end is bounds of inner content. For example: (example 4292 4486)."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (skip-chars-forward "\s\t")
      (let ((case-fold-search t)
            (block-start-re
             "[,]?#\\+\\(begin\\)_\\([a-z]+\\)\\($\\|[\s\f\t\n\r\v]\\)")
            (prefix))
        (if (looking-at block-start-re)
            (let ((open-block (match-string-no-properties 0))
                  (block-type (match-string-no-properties 2))
                  (content-beg (progn (forward-line 1)
                                      (point))))
              (setq block-type (downcase block-type))
              (setq prefix (if (string= "," (substring open-block 0 1)) "," ""))
              (when (re-search-forward (concat
                                        prefix "#\\+\\(end\\)_" "\\("
                                        (regexp-quote
                                         (downcase block-type))
                                        "\\)" "\\($\\|[\s\f\t\n\r\v]\\)")
                                       nil t 1)
                (let ((content-end (match-beginning 0)))
                  (list block-type
                        content-beg
                        content-end))))
          (when (re-search-forward
                 "[,]?#\\+\\(begin\\|end\\)_\\([a-z]+\\)\\($\\|[\s\f\t\n\r\v]\\)"
                 nil t 1)
            (when-let ((word (match-string-no-properties 1))
                       (prefix (if (string= ","
                                            (substring-no-properties
                                             (match-string-no-properties 0)
                                             0 1))
                                   ","
                                 ""))
                       (structure-type (match-string-no-properties 2))
                       (end (match-beginning 0)))
              (when (string= (downcase word) "end")
                (when (re-search-backward (concat prefix
                                                  "#\\+\\(begin\\)_" "\\("
                                                  (regexp-quote
                                                   (downcase structure-type))
                                                  "\\)"
                                                  "\\($\\|[\s\f\t\n\r\v]\\)")
                                          nil t 1)
                  (forward-line 1)
                  (list (downcase structure-type)
                        (point)
                        end))))))))))

(defun org-extra-overlay-flash-region (start end &optional face timeout)
  "Temporarily highlight region from START to END with FACE.
Default value of TIMEOUT is 0.2 seconds."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face (or face 'diary))
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun org-extra-overlay-prompt-region (beg end fn &rest args)
  "Highlight region from BEG to END while invoking FN with ARGS."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn (overlay-put overlay 'face 'diary)
               (apply fn args))
      (delete-overlay overlay))))

(defun org-extra-call-with-overlays (alist-bounds fn &rest args)
  "Highlight region from ALIST-BOUNDS while invoking FN with ARGS."
  (let ((overlays (mapcar (lambda (it)
                            (make-overlay (car it)
                                          (cdr it)))
                          alist-bounds)))
    (unwind-protect
        (progn
          (dolist (overlay overlays)
            (overlay-put overlay 'face 'diary))
          (apply fn args))
      (dolist (overlay overlays)
        (delete-overlay overlay)))))

(defun org-extra-read-language (code)
  "Read org language using detected language in CODE as initial input.
This function use library `language-detection'."
  (let ((detected-lang (when (and
                              (require 'language-detection nil t)
                              (fboundp 'language-detection-string))
                         (alist-get (language-detection-string code)
                                    org-extra-languages-alist))))
    (org-extra-read-babel-languages "Language: " nil nil
                                    detected-lang
                                    nil detected-lang)))

;;;###autoload
(defun org-extra-example-block-to-src (&optional language suffix)
  "Convert example block at point to begin/end_SUFFIX with LANGUAGE.
If LANGUAGE is omitted, read it with completions."
  (interactive)
  (save-excursion
    (when-let ((info (org-extra-bounds-of-current-block)))
      (let ((code-start (nth 1 info))
            (code-end (nth 2 info))
            (type (downcase (car info)))
            (content)
            (rep-beg)
            (rep-end)
            (prefix)
            (suffix suffix)
            (alist-bounds))
        (setq rep-beg (save-excursion
                        (goto-char code-start)
                        (forward-line -1)
                        (skip-chars-forward "\s\t")
                        (point)))
        (setq prefix (if (string= "," (buffer-substring-no-properties
                                       rep-beg
                                       (1+ rep-beg)))
                         "," ""))
        (setq rep-end (save-excursion
                        (goto-char code-end)
                        (let ((case-fold-search t))
                          (re-search-forward
                           (concat "[,]?#\\+\\(begin\\|end\\)_" type
                                   "\\($\\|[\s\f\t\n\r\v]\\)")
                           nil t 1))))
        (setq content (buffer-substring-no-properties code-start code-end))
        (setq alist-bounds (list (cons (save-excursion
                                         (goto-char rep-beg)
                                         (line-end-position))
                                       rep-beg)
                                 (cons
                                  (save-excursion
                                    (goto-char rep-end)
                                    (line-beginning-position))
                                  rep-end)))
        (setq suffix
              (or suffix (org-extra-call-with-overlays
                          alist-bounds
                          (lambda ()
                            (completing-read
                             "Replace with"
                             (mapcar #'cdr
                                     org-structure-template-alist))))))
        (pcase suffix
          ("src" (setq suffix (concat
                               suffix " "
                               (or language
                                   (org-extra-overlay-prompt-region
                                    code-start code-end
                                    (lambda ()
                                      (org-extra-read-language
                                       content))))))))
        (replace-region-contents
         rep-beg
         rep-end
         (lambda ()
           (concat
            (string-join
             (delete nil (list prefix "#+begin_" suffix)) "")
            "\n"
            content
            (string-join
             (delete nil
                     (list prefix "#+end_"
                           (car
                            (split-string suffix nil t))))))))))))

;;;###autoload
(defun org-extra-example-blocks-to-org (&optional language suffix)
  "Convert example blocks in buffer to begin/end_SUFFIX blocks with LANGUAGE.
If LANGUAGE is omitted, read it with completions."
  (interactive)
  (org-with-wide-buffer
   (widen)
   (goto-char (point-max))
   (while (re-search-backward "#\\+\\(begin\\)_example" nil t 1)
     (org-extra-example-block-to-src language suffix))))

;;;###autoload
(defun org-extra-add-names-to-src-blocks ()
  "Add names to all src blocks if package `org-extra-complete' installed."
  (interactive)
  (require 'org-extra-complete nil t)
  (widen)
  (org-babel-map-src-blocks buffer-read-only
    (let ((case-fold-search t))
      (goto-char beg-block)
      (recenter-top-bottom)
      (while (and
              (not (bobp))
              (looking-at "[\s\t]*#\\+")
              (not (looking-at "[\s\t]*#\\+name:")))
        (forward-line -1))
      (unless (looking-at "[\s\t]*#\\+name:[\s\t][a-z0-9]+")
        (org-fold-show-all)
        (if (looking-at "[\s\t]*#\\+name:")
            (progn
              (re-search-forward "#\\+name:" nil t 1)
              (skip-chars-forward "\s\t")
              (if (fboundp 'org-extra-complete)
                  (org-extra-complete)
                (insert (concat (if (looking-back ":" 0) " " "")
                                (read-string "#\\+name: ")))))
          (when-let ((name (org-extra-overlay-prompt-region
                            (line-beginning-position)
                            (line-end-position)
                            (lambda ()
                              (if (fboundp 'org-extra-complete-name)
                                  (org-extra-complete-name)
                                (read-string "#\\+name: "))))))
            (insert (concat "\n" "#+name: " name (if (looking-at "[\s\t]*\n")
                                                     ""
                                                   "\n")))))))))

(defun org-extra-get-html-head ()
  "Return string with custom styles for `org-html-head'."
  (let ((content (with-temp-buffer
                   (erase-buffer)
                   (insert-file-contents (expand-file-name
                                          "org-html-head.css"
                                          org-extra-preview-data-root))
                   (buffer-string))))
    (format "<style type=\"text/css\">\n%s</style>" content)))

(defun org-extra-get-html-scripts ()
  "Return string with custom styles for `org-html-scripts'."
  (let ((content (with-temp-buffer
                   (erase-buffer)
                   (insert-file-contents (expand-file-name
                                          "org-html-scripts.js"
                                          org-extra-preview-data-root))
                   (buffer-string))))
    (format "<script type=\"text/javascript\">\n%s</script>" content)))

;;;###autoload
(defun org-extra-narrow-to-block-content ()
  "Narrow to inner content of current block."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (pcase-let ((`(,_type ,beg ,end)
                 (org-extra-bounds-of-current-block)))
      (narrow-to-region beg end))))

(defun org-extra--bar-make-toggle-description (description value &optional
                                                           on-label off-label
                                                           left-separator
                                                           right-separator)
  "Enhance DESCRIPTION for VALUE with ON-LABEL or OFF-LABEL.
Wraps result in LEFT-SEPARATOR and RIGHT-SEPARATOR."
  (let* ((description (or description ""))
         (align (apply #'max (list (+ 5 (length description))
                                   45))))
    (concat
     (or description "")
     (propertize " " 'display (list 'space :align-to align))
     (or left-separator "")
     (if value
         (propertize
          (or on-label "+")
          'face
          'success)
       (propertize
        (or off-label "-")
        'face
        'transient-inactive-value))
     (or right-separator ""))))

;;;###autoload (autoload 'org-extra-org-mode-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-org-mode-menu ()
  "Transient menu for Org commands."
  [("s" "Show/Hide" org-extra-show-hide-menu)
   ("n" "New Heading" org-insert-heading)
   ("a" "Navigate Headings" org-extra-navigate-headings-menu)
   ("e" "Edit Structure" org-extra-edit-structure-menu)
   ("d" "Editing" org-extra-editing-menu)
   ("r" "Archive" org-extra-archive-menu)
   ("h" "Hyperlinks" org-extra-hyperlinks-menu)
   ("t" "TODO Lists" org-extra-todo-lists-menu)
   ("g" "TAGS and Properties" org-extra-tags-and-properties-menu)
   ("c" "Dates and Scheduling" org-extra-dates-and-scheduling-menu)
   ("l" "Logging work" org-extra-logging-work-menu)
   ("o" "Agenda Command... (C-c A)" org-agenda)
   ("i" "Set Restriction Lock (C-c C-x <)" org-agenda-set-restriction-lock)
   ("f" "File List for Agenda" org-extra-file-list-for-agenda-menu)
   ("p" "Special views current file" org-extra-special-views-current-file-menu)
   ("x" "Export/Publish... (C-c C-e)" org-export-dispatch)
   ("b" "LaTeX" org-extra-latex-menu)
   ("u" "Documentation" org-extra-documentation-menu)
   ("m" "Customize" org-extra-customize-menu)
   ("j" "Send bug report" org-submit-bug-report)
   ("k" "Refresh/Reload" org-extra-refresh-reload-menu)])

;;;###autoload (autoload 'org-extra-refresh-reload-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-refresh-reload-menu ()
  "Transient menu for Refresh/Reload commands."
  [("r" "Refresh setup current buffer" org-mode-restart)
   ("e" "Reload Org (after update) (C-c C-x !)" org-reload)
   ("l" "Reload Org uncompiled" (lambda ()
                                  (interactive)
                                  (org-reload t)))])

;;;###autoload (autoload 'org-extra-customize-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-customize-menu ()
  "Transient menu for Customize commands."
  [("b" "Browse Org Group" org-customize)])

;;;###autoload (autoload 'org-extra-documentation-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-documentation-menu ()
  "Transient menu for Documentation commands."
  [("s" "Show Version" org-version)
   ("i" "Info Documentation" org-info)
   ("b" "Browse Org News" org-browse-news)])

;;;###autoload (autoload 'org-extra-latex-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-latex-menu ()
  "Transient menu for LaTeX commands."
  [("o" org-cdlatex-mode
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Org CDLaTeX mode"
                                                            org-cdlatex-mode "+"
                                                            "" "[" "]"))
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (require 'cdlatex nil t)))
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Org CDLaTeX mode"
                                                            org-cdlatex-mode "+"
                                                            "" "[" "]"))
    :transient t)
   ("m" "Modify math symbol" org-cdlatex-math-modify :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-inside-LaTeX-fragment-p))))
   ("s" "Insert citation (C-c C-x [)" org-reftex-citation)])

;;;###autoload (autoload 'org-extra-special-views-current-file-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-special-views-current-file-menu ()
  "Transient menu for Special views current file commands."
  [("t" "TODO Tree" org-show-todo-tree)
   ("c" "Check Deadlines" org-check-deadlines)
   ("a" "Tags/Property tree (C-c \\)" org-match-sparse-tree)])

;;;###autoload (autoload 'org-extra-file-list-for-agenda-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-file-list-for-agenda-menu ()
  "Transient menu for File List for Agenda commands."
  [("e" "Edit File List" org-edit-agenda-file-list)
   ("a" "Add/Move Current File to Front of List (C-c [)"
    org-agenda-file-to-front)
   ("r" "Remove Current File from List (C-c ])" org-remove-file)
   ("c" "Cycle through agenda files" org-cycle-agenda-files :transient t)
   ("o" "Occur in all agenda files" org-occur-in-agenda-files)])

;;;###autoload
(defun org-extra-logging-record-done-time ()
  "Toggle `org-log-done'."
  (interactive)
  (progn (setq org-log-done (not org-log-done))
	       (message "Switching to %s will %s record a timestamp"
		              (car org-done-keywords)
		              (if org-log-done "automatically" "not"))))
;;;###autoload (autoload 'org-extra-logging-work-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-logging-work-menu ()
  "Transient menu for Logging work commands."
  [("c" "Clock in (C-c C-x TAB)" org-clock-in)
   ("s" "Switch task" (lambda ()
                        (interactive)
                        (org-clock-in '(4))))
   ("l" "Clock out (C-c C-x C-o)" org-clock-out)
   ("o" "Clock cancel (C-c C-x C-q)" org-clock-cancel)
   ("m" "Mark as default task" org-clock-mark-default-task)
   ("k" "Clock in, mark as default" (lambda ()
                                      (interactive)
                                      (org-clock-in '(16))))
   ("g" "Goto running clock (C-c C-x C-j)" org-clock-goto)
   ("d" "Display times (C-c C-x C-d)" org-clock-display)
   ("r" "Create clock table" org-clock-report)
   ("e" org-extra-logging-record-done-time
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Record DONE time"
                                                            org-log-done "+" ""
                                                            "[" "]"))
    :transient t)])
;;;###autoload (autoload 'org-extra-dates-and-scheduling-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-dates-and-scheduling-menu ()
  "Transient menu for Dates and Scheduling commands."
  [("t" "Timestamp (C-c .)" org-time-stamp :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-before-first-heading-p)))))
   ("i" "Timestamp (inactive) (C-c !)" org-time-stamp-inactive :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-before-first-heading-p)))))
   ("c" "Change Date" org-extra-change-date-menu)
   ("o" "Compute Time Range (C-c C-y)" org-evaluate-time-range)
   ("s" "Schedule Item" org-schedule :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-before-first-heading-p)))))
   ("d" "Deadline (C-c C-d)" org-deadline :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-before-first-heading-p)))))
   ("u" org-toggle-time-stamp-overlays
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Custom time format"
                                                            org-display-custom-times
                                                            "*" "" "(" ")"))
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Custom time format"
                                                            org-display-custom-times
                                                            "*" "" "(" ")"))
    :transient t)
   ("g" "Goto Calendar (C-c >)" org-goto-calendar)
   ("a" "Date from Calendar (C-c <)" org-date-from-calendar)
   ("r" "Start/Restart Timer (C-c C-x 0)" org-timer-start)
   ("p" "Pause/Continue Timer (C-c C-x ,)" org-timer-pause-or-continue)
   ("m" "Stop Timer (C-c C-x ,)" org-timer-pause-or-continue)
   ("n" "Insert Timer String (C-c C-x .)" org-timer)
   ("e" "Insert Timer Item (C-c C-x -)" org-timer-item)])
;;;###autoload (autoload 'org-extra-change-date-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-change-date-menu ()
  "Transient menu for Change Date commands."
  [("d" "1 Day Later (S-<right>)" org-shiftright :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-timestamp-p 'lax))))
   ("a" "1 Day Earlier (S-<left>)" org-shiftleft :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-timestamp-p 'lax))))
   ("l" "1 ... Later (S-<up>)" org-shiftup :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-timestamp-p 'lax))))
   ("e" "1 ... Earlier (S-<down>)" org-shiftdown :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-timestamp-p 'lax))))])
;;;###autoload (autoload 'org-extra-tags-and-properties-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-tags-and-properties-menu ()
  "Transient menu for TAGS and Properties commands."
  [("s" "Set Tags (C-c C-q)" org-set-tags-command :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-before-first-heading-p)))))
   ("c" "Change tag in region" org-change-tag-in-region :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-region-active-p))))
   ("e" "Set property (C-c C-x p)" org-set-property :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-before-first-heading-p)))))
   ("o" "Column view of properties (C-c C-x C-c)" org-columns)
   ("i" "Insert Column View DBlock" org-columns-insert-dblock)])
;;;###autoload (autoload 'org-extra-todo-lists-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-todo-lists-menu ()
  "Transient menu for TODO Lists commands."
  [("t" "TODO/DONE/- (C-c C-t)" org-todo :transient t)
   ("s" "Select keyword" org-extra-select-keyword-menu)
   ("h" "Show TODO Tree" org-show-todo-tree :transient t)
   ("g" "Global TODO list (C-c k t)" org-todo-list)
   ("e" (lambda ()
          (interactive)
          (customize-variable 'org-enforce-todo-dependencies))
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description
       "Enforce dependencies" org-enforce-todo-dependencies "+" "" "[" "]")))
   ("d" org-toggle-ordered-property
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description
       "Do Children sequentially"
       (org-entry-get
        nil
        "ORDERED")
       "*" ""
       "(" ")"))
    :inapt-if-not
    (lambda ()
      (ignore-errors org-enforce-todo-dependencies))
    :transient t)
   ("o" org-toggle-ordered-property
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description
       "Do Children parallel"
       (not
        (org-entry-get
         nil
         "ORDERED"))
       "*" ""
       "(" ")"))
    :inapt-if-not
    (lambda ()
      (ignore-errors org-enforce-todo-dependencies))
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description
       "Do Children parallel"
       (not
        (org-entry-get
         nil
         "ORDERED"))
       "*" ""
       "(" ")"))
    :transient t)
   ("p" "Set Priority (C-c ,)" org-priority)
   ("r" "Priority Up (S-<up>)" org-shiftup :transient t)
   ("i" "Priority Down (S-<down>)" org-shiftdown :transient t)
   ("n" "Get news from all feeds (C-c C-x g)" org-feed-update-all)
   ("b" "Go to the inbox of a feed... (C-c C-x G)" org-feed-goto-inbox)
   ("c" "Customize feeds" (lambda ()
                            (interactive)
                            (customize-variable 'org-feed-alist)))])

;;;###autoload (autoload 'org-extra-select-keyword-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-select-keyword-menu ()
  "Transient menu for Select keyword commands."
  [("n" "Next keyword (S-<right>)" org-shiftright
    :transient t
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-heading-p))))
   ("p" "Previous keyword (S-<left>)" org-shiftleft
    :transient t
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-heading-p))))
   ("c" "Complete Keyword" pcomplete :inapt-if-not
    (lambda ()
      (ignore-errors
        (assq :todo-keyword
              (org-context)))))
   ("e" "Next keyword set (C-S-<right>)" org-shiftcontrolright
    :transient t
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (and
         (>
          (length org-todo-sets)
          1)
         (org-at-heading-p)))))
   ("r" "Previous keyword set (C-S-<right>)" org-shiftcontrolright
    :transient t
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (and
         (>
          (length org-todo-sets)
          1)
         (org-at-heading-p)))))])
;;;###autoload (autoload 'org-extra-hyperlinks-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-hyperlinks-menu ()
  "Transient menu for Hyperlinks commands."
  [("s" "Store Link (Global)" org-store-link)
   ("f" "Find existing link to here" org-occur-link-in-agenda-files)
   ("i" "Insert Link (C-c C-l)" org-insert-link)
   ("o" "Follow Link (C-c C-o)" org-open-at-point)
   ("n" "Next link (M-n)" org-next-link
    :transient t)
   ("p" "Previous link (M-p)" org-previous-link
    :transient t)
   ("d" org-toggle-link-display
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Descriptive Links"
                                                            org-link-descriptive
                                                            "*" "" "(" ")"))
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Descriptive Links"
                                                            org-link-descriptive
                                                            "*" "" "(" ")"))
    :transient t)
   ("l" org-toggle-link-display
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Literal Links"
                                                            (not
                                                             org-descriptive-links)
                                                            "*" ""
                                                            "(" ")"))
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Literal Links"
                                                            (not
                                                             org-link-descriptive)
                                                            "*" ""
                                                            "(" ")"))
    :transient t)])

;;;###autoload (autoload 'org-extra-archive-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-archive-menu ()
  "Transient menu for Archive commands."
  [("a" "Archive (default method) (C-c C-x C-a)" org-archive-subtree-default
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("m" "Move Subtree to Archive file (C-c $)" org-archive-subtree :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("t" "Toggle ARCHIVE tag (C-c C-x a)" org-toggle-archive-tag :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("o" "Move subtree to Archive sibling (C-c C-x A)"
    org-archive-to-archive-sibling :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))])
;;;###autoload (autoload 'org-extra-editing-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-editing-menu ()
  "Transient menu for Editing commands."
  [("e" "Emphasis... (C-c C-x C-f)" org-emphasize)
   ("a" "Add block structure (C-c C-,)" org-insert-structure-template)
   ("d" "Edit Source Example (C-c ')" org-edit-special)
   ("f" "Footnote new/jump (C-c C-x f)" org-footnote-action)
   ("o" "Footnote extra" (lambda ()
                           (interactive)
                           (org-footnote-action t)))])

;;;###autoload (autoload 'org-extra-edit-structure-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-edit-structure-menu ()
  "Transient menu for Edit Structure commands."
  [("m" "Move Subtree Up (M-<up>)" org-metaup
    :transient t
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-heading-p))))
   ("o" "Move Subtree Down (M-<down>)" org-metadown
    :transient t
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-at-heading-p))))
   ("c" "Copy Subtree (C-c C-x M-w)" org-copy-special :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("u" "Cut Subtree (C-c C-x C-w)" org-cut-special :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("p" "Paste Subtree (C-c C-x C-y)" org-paste-special :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-at-table-p)))))
   ("l" "Clone subtree, shift time (C-c C-x c)"
    org-clone-subtree-with-time-shift)
   ("y" "Copy visible text (C-c C-x v)" org-copy-visible)
   ("r" "Promote Heading (M-<left>)" org-metaleft :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("t" "Promote Subtree (M-S-<left>)" org-shiftmetaleft :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("d" "Demote Heading (M-<right>)" org-metaright :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("e" "Demote Subtree (M-S-<right>)" org-shiftmetaright :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("s" "Sort Region/Children (C-c ^)" org-sort)
   ("n" "Convert to odd levels" org-convert-to-odd-levels)
   ("v" "Convert to odd/even levels" org-convert-to-oddeven-levels)
   ("f" "Refile Subtree (C-c C-w)" org-refile :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("i" "Refile and copy Subtree (C-c M-w)" org-refile-copy :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))])

;;;###autoload (autoload 'org-extra-navigate-headings-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-navigate-headings-menu ()
  "Transient menu for Navigate Headings commands."
  [("u" "Up (C-c C-u)" outline-up-heading
    :transient t)
   ("n" "Next (C-c C-n)" outline-next-visible-heading
    :transient t)
   ("p" "Previous (C-c C-p)" outline-previous-visible-heading
    :transient t)
   ("e" "Next Same Level (C-c C-f)" outline-forward-same-level
    :transient t)
   ("r" "Previous Same Level (C-c C-b)" outline-backward-same-level
    :transient t)
   ("j" "Jump (C-c C-j)" org-goto)])

;;;###autoload (autoload 'org-extra-show-hide-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-show-hide-menu ()
  "Transient menu for Show/Hide commands."
  [("c" "Cycle Visibility (TAB)" org-cycle :inapt-if-not
    (lambda ()
      (ignore-errors
        (or
         (bobp)
         (outline-on-heading-p)))))
   ("y" "Cycle Global Visibility (<backtab>)" org-shifttab :inapt-if-not
    (lambda ()
      (ignore-errors
        (not
         (org-at-table-p)))))
   ("s" "Sparse Tree... (C-c /)" org-sparse-tree)
   ("r" "Reveal Context (C-c C-r)" org-fold-reveal)
   ("h" "Show All" org-fold-show-all)
   ("u" "Subtree to indirect buffer (C-c C-x b)" org-tree-to-indirect-buffer)])

;;;###autoload (autoload 'org-extra-menu-babel-transient "org-extra.el" nil t)
(transient-define-prefix org-extra-menu-babel-transient ()
  "Command dispatcher for org babel."
  :transient-suffix #'transient--do-call
  :transient-non-suffix #'transient--do-stay
  ["Babel"
   ["Go"
    ("n" "next src block" org-babel-next-src-block)
    ("p" "previous src block" org-babel-previous-src-block)
    ("g" "named src block" org-babel-goto-named-src-block)
    ("u" "goto src block head" org-babel-goto-src-block-head)
    ("r" "to result" org-babel-goto-named-result)]
   ["Edit"
    ("j" "insert header arg" org-babel-insert-header-arg)
    ("h" "mark block" org-babel-mark-block)
    ("v" "expand src block" org-babel-expand-src-block)
    ("e" "Wrap or split" org-babel-demarcate-block)]
   ["Result"
    ("k" "remove current" org-babel-remove-result-one-or-many)
    ("D" "remove all"
     (lambda ()
       (interactive)
       (org-babel-remove-result-one-or-many t)))
    ("o" "open src block result" org-babel-open-src-block-result)
    ("a" "Generate a sha1" org-babel-sha1-hash)]]
  [["Execute"
    ("b" "execute buffer" org-babel-execute-buffer)
    ("m" "execute maybe" org-babel-execute-maybe)
    ("s" "subtree" org-babel-execute-subtree)]
   ["Session"
    ("z" "switch to session with code"
     org-babel-switch-to-session-with-code)
    ("C" "switch to session" org-babel-switch-to-session)
    ("l" "load in session" org-babel-load-in-session)]
   ["Other"
    ("c" "check src block" org-babel-check-src-block)
    ("I" "view src block info" org-babel-view-src-block-info)]]
  [["Tangle"
    ("i" "Add all named source blocks in file"
     org-babel-lob-ingest)
    ("f" "tangle file" org-babel-tangle-file)
    ("t" "tangle" org-babel-tangle)]])

;;;###autoload (autoload 'org-extra-menu-org-table-transient "org-extra.el" nil t)
(transient-define-prefix org-extra-menu-org-table-transient ()
  "Command dispatcher for org table."
  :transient-suffix #'transient--do-call
  :transient-non-suffix #'transient--do-stay
  [["Org table"
    ("i" "field info" org-table-field-info)
    ("RET" "Insert row"
     (lambda nil
       (interactive)
       (org-table-insert-row t)))
    ("w" "Copy current to value one row below"
     org-table-copy-down)
    ("o" "Rotate the recalculation mark in the first column"
     org-table-rotate-recalc-marks)
    ("e" "edit field" org-table-edit-field)
    ("s" "Sum" org-table-sum)
    ("v" "Eval formula" org-table-eval-formula)
    ("a" "Create with table el" org-table-create-with-table.el)]
   [("f" "Toggle the formula debugger in tables"
     org-table-toggle-formula-debugger)
    ("t" "Toggle coordinate overlays"
     org-table-toggle-coordinate-overlays)]]
  (interactive)
  (if (when (fboundp 'org-at-table-p)
        (org-at-table-p))
      (transient-setup 'org-extra-menu-org-table-transient)
    (if (when (fboundp 'org-region-active-p)
          (org-region-active-p))
        (org-table-convert-region
         (region-beginning)
         (region-end)
         (pcase (car (read-multiple-choice ""
                                           '((?,",")
                                             (?t "TAB")
                                             (?r "a regular expression")
                                             (?i "integer (number of spaces or tabs)")
                                             (?n "auto")
                                             (?q "auto"))))
           (?,'(4))
           (?t '(16))
           (?r '(64))
           (?i (read-number
                "A number to use that many spaces, or a TAB, as field separator"
                2))
           ((or ?n ?q) nil)))
      (org-table-create (format "%sx%s"
                                (read-number "Columns: " 2)
                                (read-number "Rows: " 4))))))

;;;###autoload (autoload 'org-extra-menu-clock "org-extra.el" nil t)
(transient-define-prefix org-extra-menu-clock ()
  "Command dispatcher for org clocks."
  ["Clock in/out"
   [("i" "Clock in" org-clock-in)
    ("c" "Continiue" org-clock-in-last)
    ("o" "Out" org-clock-out)
    ("q" "Cancel" org-clock-cancel)
    ("e" "Change effort" org-clock-modify-effort-estimate)]
   ["Summary"
    ("g" "Goto " org-clock-goto)
    ("d" "Show subtree timers" org-clock-display)
    ("r" "Report" org-clock-report)]
   ["Help"
    ("z" "Info about Timers"
     (lambda ()
       (interactive)
       (org-info "Timers")))
    ("?" "Show info"
     (lambda ()
       (interactive)
       (org-info "Clocking commands")))]]
  [["Timer"
    ("t" "Insert" org-timer-start)
    ("n" "Set" org-timer-set-timer)
    ("p" "Pause or continue" org-timer-pause-or-continue)
    ("s" "Stop" org-timer-stop)]
   ["Insert"
    ("m" "Insert time from timer" org-timer)
    ("I"
     "Insert a description-type item with the current timer value"
     org-timer-item)
    ("h" "Schedule" org-schedule)]])


(defun org-extra-agenda-archives-files ()
  "Toggle inclusion of files in trees marked with :ARCHIVE:."
  (interactive)
  (require 'org-agenda)
  (when (fboundp 'org-agenda-archives-mode)
    (org-agenda-archives-mode 'files)))

(transient-define-prefix org-extra-agenda-transient ()
  "Command dispatcher for agenda mode."
  [["Headline"
    ("h A" "Archive the entry or subtree"
     org-agenda-archive-default)
    ("h k" "Kill the entry or subtree" org-agenda-kill)
    ("h p" "Set the priority" org-agenda-priority)
    ("h r" "Refile the item at point" org-agenda-refile)
    ("h :" "Set tags for the current headline"
     org-agenda-set-tags)
    ("h t" "Cycle TODO state" org-agenda-todo)]
   ["Visit"
    ("TAB" "Go to the entry" org-agenda-goto)
    ("SPC" "Show and scroll" org-agenda-show-and-scroll-up)
    ("RET" "Select current" org-agenda-switch-to)]
   ["Date"
    ("d s" "Schedule the item at point" org-agenda-schedule)
    ("d t" "Change the date" org-agenda-date-prompt)
    ("d d" "Schedule the item at point" org-agenda-deadline)
    ("+" "Do date later" org-agenda-do-date-later)
    ("-" "Do date earlier" org-agenda-do-date-earlier)]
   ["View"
    ("v d" "Daily" org-agenda-day-view)
    ("v w" "Weekly " org-agenda-week-view)
    ("v t" "Fortnightly" org-agenda-fortnight-view)
    ("v m" "Monthly" org-agenda-month-view)
    ("v y" "Yearly" org-agenda-year-view)
    ("v r" "Default" org-agenda-reset-view)
    ("v n" "Go forward in time" org-agenda-later)
    ("v p" "Go backward in time" org-agenda-earlier)]]
  [["Toggle"
    ("t r" "Clocktable" org-agenda-clockreport-mode)
    ("t f" "Follow" org-agenda-follow-mode)
    ("t l" "Log mode" org-agenda-log-mode)
    ("t a" "Archive trees" org-agenda-archives-mode)
    ("t A" "Archive files"
     org-extra-agenda-archives-files)
    ("t d" "Diary" org-agenda-toggle-diary)]
   ["Filter"
    ("f c" "By category" org-agenda-filter-by-category)
    ("f x" "by a regexp" org-agenda-filter-by-regexp)
    ("f t" "by tag" org-agenda-filter-by-tag)
    ("f h" "By headline" org-agenda-filter-by-top-headline)
    ("f d" "Reset" org-agenda-filter-remove-all)]
   ["Clock"
    ("c j" "Goto clocked task" org-agenda-clock-goto)
    ("c i" "Start clock" org-agenda-clock-in)
    ("c q" "Cancel running clock" org-agenda-clock-cancel)
    ("c o" "Stop running clock" org-agenda-clock-out)]
   ["Other"
    ("g d" "Jump to date" org-agenda-goto-date)
    ("." "Go to today" org-agenda-goto-today)
    ("g r" "Rebuild views " org-agenda-redo)]])

(provide 'org-extra)
;;; org-extra.el ends here