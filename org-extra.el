;;; org-extra.el --- Configure extra -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-extra
;; Keywords: outlines
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (org "9.6.11") (transient "0.4.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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
(require 'transient)

(defvar org-src-block-faces)
(defvar org-archive-default-command)

(defvar org-columns-current-fmt)
(declare-function org-columns-quit "org-colview")

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
                                       (emacslisp . "elisp")
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

(defvar org-src-lang-modes)
(defvar org-babel-load-languages)

(defun org-extra-ob-packages ()
  "List all `org-babel' languages in `features' excluding core and autoloads."
  (let ((langs))
    (dolist (it features)
      (let ((name (symbol-name it)))
        (when (and
               (string-prefix-p "ob-" name)
               (not (member name '("ob-core" "ob-eval" "ob-table" "ob-tangle"
                                   "ob-comint"
                                   "ob-exp"
                                   "ob-lob"
                                   "ob-ref")))
               (not (string-suffix-p "autoloads" name)))
          (push (substring-no-properties name 3) langs))))
    langs))

(defun org-extra-get-all-external-babel-languages ()
  "Retrieve all external Babel languages supported by Org mode."
  (let* ((suffixes (find-library-suffixes))
         (regexp (concat (regexp-opt suffixes) "\\'"))
         (dirs (seq-remove
                (apply-partially #'string-suffix-p "org")
                load-path))
         (files nil))
    (dolist (dir dirs)
      (dolist (file (ignore-errors (directory-files dir nil regexp t)))
        (when (string-match regexp file)
          (let ((name (substring file 0 (match-beginning 0))))
            (when (and (string-prefix-p "ob-" name)
                       (not
                        (or (string-suffix-p "-autoloads" name)
                            (string-suffix-p "-mode" name))))
              (push (substring name 3) files))))))
    files))

(defun org-extra-get-all-babel-languages ()
  "Retrieve all languages supported by Babel in Org mode."
  (delete-dups
   (mapcar (apply-partially #'format "%s")
           (append
            (mapcar #'car org-babel-load-languages)
            (mapcar #'car org-src-lang-modes)
            (mapcar (pcase-lambda (`(,_t ,_k ,_label ,sym)) sym)
                    (cdadr (memq :key-type
                                 (get
                                  'org-babel-load-languages
                                  'custom-type))))
            (org-extra-get-all-external-babel-languages)
            (org-extra-ob-packages)))))

(defun org-extra-babel-load-all-languages ()
  "Load all Babel languages using the `org-extra-babel-load-language' function."
  (dolist (lang-str (org-extra-get-all-babel-languages))
    (org-extra-babel-load-language lang-str)))

(defun org-extra-read-babel-languages (prompt &optional predicate require-match
                                              initial-input hist def
                                              inherit-input-method)
  "Read org babel language with PROMPT.
Optional arguments PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF and
INHERIT-INPUT-METHOD have the same meaning as for `completing-read'."
  (let* ((langs (org-extra-get-all-babel-languages))
         (annotf (lambda (str)
                   (format " (%s)"
                           (org-src-get-lang-mode str)))))
    (completing-read prompt
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action langs str pred)))
                     predicate require-match
                     initial-input hist def
                     inherit-input-method)))

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
          (require (intern (concat "ob-" (symbol-name found)))))))))

(defun org-extra-babel-before-execute-src-block (&rest args)
  "Advice function for `org-babel-execute-src-block' to load language from ARGS.

Usage:


\\=(advice-add \\='org-babel-execute-src-block
              :before #\\='org-extra-babel-before-execute-src-block)."
  (pcase-let ((`(,_arg ,info . ,_params) args))
    (org-extra-babel-load-language (nth 0 info))))

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

(defcustom org-extra-eldoc-flags-functions '(org-extra-eldoc-documentation-function)
  "List of additional eldoc functions."
  :group 'org-extra
  :type '(repeat
          (radio
           (function-item elisp-eldoc-var-docstring)
           (function-item org-extra-eldoc-documentation-function)
           (function :tag "Custom function"))))

;;;###autoload
(define-minor-mode org-extra-eldoc-mode
  "Add more eldoc functions when this mode is turned on."
  :lighter " org-eldoc+"
  :global nil
  (if org-extra-eldoc-mode
      (dolist (fn org-extra-eldoc-flags-functions)
        (add-hook 'eldoc-documentation-functions fn nil t))
    (dolist (fn org-extra-eldoc-flags-functions)
      (remove-hook 'eldoc-documentation-functions fn t))))

(defcustom org-extra-eldoc-breadcrumb-separator "/"
  "Breadcrumb separator."
  :group 'org-eldoc
  :type 'string)

(defcustom org-extra-eldoc-test-buffer-name " *Org-eldoc test buffer*"
  "Name of the buffer used while testing for mode-local variable values."
  :group 'org-eldoc
  :type 'string)

(defun org-extra-eldoc-get-breadcrumb ()
  "Return breadcrumb if on a headline or nil."
  (let ((case-fold-search t) cur)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at org-complex-heading-regexp)
          (setq cur (match-string 4))
          (org-format-outline-path
           (append (org-get-outline-path)
                   (list cur))
           (frame-width) "" org-extra-eldoc-breadcrumb-separator))))))

(defun org-extra-eldoc-get-src-header ()
  "Return lang and list of header properties if on src definition line."
  (let ((case-fold-search t) info lang hdr-args)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
          (setq info (org-babel-get-src-block-info 'light)
                lang (propertize (or (nth 0 info) "no lang") 'face
                                 'font-lock-string-face)
                hdr-args (nth 2 info))
          (concat
           lang
           ": "
           (mapconcat
            (lambda (elem)
              (or
               (when-let ((val (and (cdr elem)
                                    (format "%s" (cdr elem)))))
                 (when (not (string-empty-p val))
                   (concat
                    (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                    " "
                    (propertize val 'face 'org-verbatim)
                    " ")))
               ""))
            hdr-args " ")))))))

(declare-function org-element-type "org")
(declare-function org-element-property "org")

(defun org-extra-eldoc-get-src-lang ()
  "Return value of lang for the current block if in block body and nil otherwise."
  (let ((element (save-match-data
                   (org-element-at-point))))
    (and (eq (org-element-type element) 'src-block)
         (>= (line-beginning-position)
             (org-element-property :post-affiliated element))
         (<=
          (line-end-position)
          (org-with-wide-buffer
           (goto-char (org-element-property :end element))
           (skip-chars-backward " \t\n")
           (line-end-position)))
         (org-element-property :language element))))

(defvar org-extra-eldoc-local-functions-cache (make-hash-table
                                               :size 40
                                               :test 'equal)
  "Cache of major-mode's `eldoc-documentation-functions'.")

(defun org-extra-eldoc-get-mode-local-documentation-function (lang)
  "Check if LANG set `eldoc-documentation-function' and return its value."
  (let ((cached-func (gethash lang org-extra-eldoc-local-functions-cache 'empty))
        (mode-func (org-src-get-lang-mode lang))
        doc-func)
    (if (eq 'empty cached-func)
        (when (fboundp mode-func)
          (with-temp-buffer
            (funcall mode-func)
            (setq doc-func (if (boundp 'eldoc-documentation-functions)
                               (let ((doc-funs eldoc-documentation-functions))
                                 (lambda (callback)
                                   (let
                                       ((eldoc-documentation-functions doc-funs))
                                     (run-hook-with-args-until-success
                                      'eldoc-documentation-functions
                                      callback))))
                             (and eldoc-documentation-function
                                  (symbol-value 'eldoc-documentation-function))))
            (puthash lang doc-func org-extra-eldoc-local-functions-cache))
          doc-func)
      cached-func)))

(declare-function c-eldoc-print-current-symbol-info "c-eldoc" ())
(declare-function css-eldoc-function "css-eldoc" ())
(declare-function php-eldoc-function "php-eldoc" ())
(declare-function go-eldoc--documentation-function "go-eldoc" ())

(defun org-extra-get-word (&optional chars)
  "Get thing at point matching CHARS.
Optional argument CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)"
  (unless chars (setq chars "_A-Za-z0-9"))
  (when-let ((bounds (save-excursion
                       (let* ((a (save-excursion
                                   (skip-chars-backward chars)
                                   (point)))
                              (b (save-excursion
                                   (skip-chars-forward chars)
                                   (point))))
                         (if (string-blank-p
                              (buffer-substring-no-properties a b))
                             nil
                           (cons a b))))))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defvar org-extra-eldoc-special-props '(("ALLTAGS" . "All tags, including inherited ones.")
                                        ("BLOCKED" . "‘t’ if task is currently blocked by children or siblings.")
                                        ("CATEGORY" . "The category of an entry.")
                                        ("CLOCKSUM" . "The sum of CLOCK intervals in the subtree.  ‘org-clock-sum’\n                 must be run first to compute the values in the current buffer.")
                                        ("CLOCKSUM_T" . "The sum of CLOCK intervals in the subtree for today.\n                 ‘org-clock-sum-today’ must be run first to compute the\n                 values in the current buffer.")
                                        ("CLOSED" . "When was this entry closed?")
                                        ("DEADLINE" . "The deadline timestamp.")
                                        ("FILE" . "The filename the entry is located in.")
                                        ("ITEM" . "The headline of the entry.")
                                        ("PRIORITY" . "The priority of the entry, a string with a single letter.")
                                        ("SCHEDULED" . "The scheduling timestamp.")
                                        ("TAGS" . "The tags defined directly in the headline.")
                                        ("TIMESTAMP" . "The first keyword-less timestamp in the entry.")
                                        ("TIMESTAMP_IA" . "The first inactive timestamp in the entry.")
                                        ("TODO" . "The TODO keyword of the entry.")))

(defvar org-extra-eldoc-short-options
  '(("'" . "Toggle smart quotes (‘org-export-with-smart-quotes’).  Depending on\n     the language used, when activated, Org treats pairs of double\n     quotes as primary quotes, pairs of single quotes as secondary\n     quotes, and single quote marks as apostrophes.")
    ("*" . "Toggle emphasized text (‘org-export-with-emphasize’).")
    ("-" . "Toggle conversion of special strings\n     (‘org-export-with-special-strings’).")
    (":" . "Toggle fixed-width sections (‘org-export-with-fixed-width’).")
    ("<" . "Toggle inclusion of time/date active/inactive stamps\n     (‘org-export-with-timestamps’).")
    ("\\n" . "Toggles whether to preserve line breaks\n     (‘org-export-preserve-breaks’).")
    ("^" . "Toggle TeX-like syntax for sub- and superscripts.  If you write\n     ‘^:{}’, ‘a_{b}’ is interpreted, but the simple ‘a_b’ is left as it\n     is (‘org-export-with-sub-superscripts’).")
    ("arch" . "Configure how archived trees are exported.  When set to ‘headline’,\n     the export process skips the contents and processes only the\n     headlines (‘org-export-with-archived-trees’).")
    ("author" . "Toggle inclusion of author name into exported file\n     (‘org-export-with-author’).")
    ("broken-links" . "Toggles if Org should continue exporting upon finding a broken\n     internal link.  When set to ‘mark’, Org clearly marks the problem\n     link in the output (‘org-export-with-broken-links’).")
    ("c" . "Toggle inclusion of ‘CLOCK’ keywords (‘org-export-with-clocks’).")
    ("creator" . "Toggle inclusion of creator information in the exported file\n     (‘org-export-with-creator’).")
    ("d" . "Toggles inclusion of drawers, or list of drawers to include, or\n     list of drawers to exclude (‘org-export-with-drawers’).")
    ("date" . "Toggle inclusion of a date into exported file\n     (‘org-export-with-date’).")
    ("e" . "Toggle inclusion of entities (‘org-export-with-entities’).")
    ("email" . "Toggle inclusion of the author’s e-mail into exported file\n     (‘org-export-with-email’).")
    ("f" . "Toggle the inclusion of footnotes (‘org-export-with-footnotes’).")
    ("H" . "Set the number of headline levels for export\n     (‘org-export-headline-levels’).  Below that level, headlines are\n     treated differently.  In most back-ends, they become list items.")
    ("inline" . "Toggle inclusion of inlinetasks (‘org-export-with-inlinetasks’).")
    ("num" . "Toggle section-numbers (‘org-export-with-section-numbers’).  When\n     set to number N, Org numbers only those headlines at level N or\n     above.  Set ‘UNNUMBERED’ property to non-‘nil’ to disable numbering\n     of heading and subheadings entirely.  Moreover, when the value is\n     ‘notoc’ the headline, and all its children, do not appear in the\n     table of contents either (see *note Table of Contents::).")
    ("p" . "Toggle export of planning information (‘org-export-with-planning’).\n     “Planning information” comes from lines located right after the\n     headline and contain any combination of these cookies: ‘SCHEDULED’,\n     ‘DEADLINE’, or ‘CLOSED’.")
    ("pri" . "Toggle inclusion of priority cookies (‘org-export-with-priority’).")
    ("prop" . "Toggle inclusion of property drawers, or list the properties to\n     include (‘org-export-with-properties’).")
    ("stat" . "Toggle inclusion of statistics cookies\n     (‘org-export-with-statistics-cookies’).")
    ("tags" . "Toggle inclusion of tags, may also be ‘not-in-toc’\n     (‘org-export-with-tags’).")
    ("tasks" . "Toggle inclusion of tasks (TODO items); or ‘nil’ to remove all\n     tasks; or ‘todo’ to remove done tasks; or list the keywords to keep\n     (‘org-export-with-tasks’).")
    ("tex" . "‘nil’ does not export; ‘t’ exports; ‘verbatim’ keeps everything in\n     verbatim (‘org-export-with-latex’).")
    ("timestamp" . "Toggle inclusion of the creation time in the exported file\n     (‘org-export-time-stamp-file’).")
    ("title" . "Toggle inclusion of title (‘org-export-with-title’).")
    ("toc" . "Toggle inclusion of the table of contents, or set the level limit\n     (‘org-export-with-toc’).")
    ("todo" . "Toggle inclusion of TODO keywords into exported text\n     (‘org-export-with-todo-keywords’).")
    ("|" . "Toggle inclusion of tables (‘org-export-with-tables’).\n\n   When exporting subtrees, special node properties can override the\nabove keywords.  These properties have an ‘EXPORT_’ prefix.  For\nexample, ‘DATE’ becomes, ‘EXPORT_DATE’ when used for a specific subtree.\nExcept for ‘SETUPFILE’, all other keywords listed above have an")))

(defvar org-extra-eldoc-short-export-setting
  '(("AUTHOR" . "The document author (‘user-full-name’).")
    ("CREATOR" . "Entity responsible for output generation\n     (‘org-export-creator-string’).")
    ("DATE" . "A date or a time-stamp(2).")
    ("EMAIL" . "The email address (‘user-mail-address’).")
    ("LANGUAGE" . "Language to use for translating certain strings\n     (‘org-export-default-language’).  With ‘#+LANGUAGE: fr’, for\n     example, Org translates ‘Table of contents’ to the French ‘Table\n     des matières’(3).")
    ("SELECT_TAGS" . "The default value is ‘(\"export\")’.  When a tree is tagged with\n     ‘export’ (‘org-export-select-tags’), Org selects that tree and its\n     subtrees for export.  Org excludes trees with ‘noexport’ tags, see\n     below.  When selectively exporting files with ‘export’ tags set,\n     Org does not export any text that appears before the first\n     headline.")
    ("EXCLUDE_TAGS" . "The default value is ‘(\"noexport\")’.  When a tree is tagged with\n     ‘noexport’ (‘org-export-exclude-tags’), Org excludes that tree and\n     its subtrees from export.  Entries tagged with ‘noexport’ are\n     unconditionally excluded from the export, even if they have an\n     ‘export’ tag.  Even if a subtree is not exported, Org executes any\n     code blocks contained there.")
    ("TITLE" . "Org displays this title.  For long titles, use multiple ‘#+TITLE’ lines.")
    ("EXPORT_FILE_NAME" . "The name of the output file to be generated.  Otherwise, Org\n     generates the file name based on the buffer name and the extension\n     based on the back-end format.\n\n   The ‘OPTIONS’ keyword is a compact form.  To configure multiple\noptions, use several ‘OPTIONS’ lines.  ‘OPTIONS’ recognizes the\nfollowing arguments.")
    ("ARCHIVE" . "Sets the archive location of the agenda file")
    ("CONSTANTS" . "Set file-local values for constants that table formulas can use")
    ("FILETAGS" . "#+FILETAGS: :tag1:tag2:tag3:")
    ("LINK" . "#+LINK: linkword replace Each line specifies one abbreviation for one link")
    ("PROPERTY" . "#+PROPERTY: Property_Name Value")
    ("ALLTAGS" . "All tags, including inherited ones.")
    ("BLOCKED" . "‘t’ if task is currently blocked by children or siblings.")
    ("CATEGORY" . "The category of an entry.")
    ("CLOCKSUM" . "The sum of CLOCK intervals in the subtree.  ‘org-clock-sum’\n                 must be run first to compute the values in the current buffer.")
    ("CLOCKSUM_T" . "The sum of CLOCK intervals in the subtree for today.\n                 ‘org-clock-sum-today’ must be run first to compute the\n                 values in the current buffer.")
    ("CLOSED" . "When was this entry closed?")
    ("DEADLINE" . "The deadline timestamp.")
    ("FILE" . "The filename the entry is located in.")
    ("ITEM" . "The headline of the entry.")
    ("PRIORITY" . "The priority of the entry, a string with a single letter.")
    ("SCHEDULED" . "The scheduling timestamp.")
    ("TAGS" . "The tags defined directly in the headline. (‘org-tag-alist’)")
    ("TIMESTAMP" . "The first keyword-less timestamp in the entry.")
    ("TIMESTAMP_IA" . "The first inactive timestamp in the entry.")
    ("TODO" . "The TODO keyword of the entry.")
    ("DESCRIPTION" . "This is the document’s description, which the HTML exporter inserts\n     it as a HTML meta tag in the HTML file.  For long descriptions, use\n     multiple ‘DESCRIPTION’ lines.  The exporter takes care of wrapping\n     the lines properly.\n\n     The exporter includes a number of other meta tags, which can be\n     customized by modifying ‘org-html-meta-tags’.")
    ("HTML_DOCTYPE" . "Specify the document type, for example: HTML5 (‘org-html-doctype’).")
    ("HTML_CONTAINER" . "Specify the HTML container, such as ‘div’, for wrapping sections\n     and elements (‘org-html-container-element’).")
    ("HTML_LINK_HOME" . "The URL for home link (‘org-html-link-home’).")
    ("HTML_LINK_UP" . "The URL for the up link of exported HTML pages\n     (‘org-html-link-up’).")
    ("HTML_MATHJAX" . "Options for MathJax (‘org-html-mathjax-options’).  MathJax is used\n     to typeset LaTeX math in HTML documents.  See *note Math formatting\n     in HTML export::, for an example.")
    ("HTML_HEAD" . "Arbitrary lines for appending to the HTML document’s head\n     (‘org-html-head’).")
    ("HTML_HEAD_EXTRA" . "More arbitrary lines for appending to the HTML document’s head\n     (‘org-html-head-extra’).")
    ("KEYWORDS" . "Keywords to describe the document’s content.  HTML exporter inserts\n     these keywords as HTML meta tags.  For long keywords, use multiple\n     ‘KEYWORDS’ lines.")
    ("LATEX_HEADER" . "Arbitrary lines for appending to the preamble; HTML exporter\n     appends when transcoding LaTeX fragments to images (see *note Math\n     formatting in HTML export::).")
    ("SUBTITLE" . "The document’s subtitle.  HTML exporter formats subtitle if
     document type is ‘HTML5’ and the CSS has a ‘subtitle’ class.")))

(defun org-extra-describe-eldoc-setting ()
  "Return description for current option in #+options."
  (when-let* ((option
               (when-let ((w (car (split-string
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))
                                   nil t))))
                 (upcase w)))
              (descr
               (or
                (when (string-prefix-p "#+" option)
                  (save-excursion
                    (skip-chars-backward "^\s\t\n")
                    (let* ((beg (point))
                           (end (skip-chars-forward "^\s\t\n:")))
                      (cdr (assoc-string (buffer-substring-no-properties
                                          beg (+ end beg))
                                         org-extra-eldoc-short-options)))))
                (let ((alist org-extra-eldoc-short-export-setting))
                  (or
                   (cdr (assoc-string (org-extra-get-word)
                                      alist))
                   (let ((str option))
                     (when (string-prefix-p "#+" str)
                       (setq str (substring-no-properties str 2)))
                     (when (string-prefix-p ":" str)
                       (setq str (substring-no-properties str 1)))
                     (when (string-suffix-p ":" str)
                       (setq str (substring-no-properties str 0
                                                          (1- (length str)))))
                     (cdr (assoc-string str
                                        alist))))))))
    (concat (propertize option 'face 'font-lock-keyword-face) ": "
            (org-extra-substitute-get-vars
             descr))))

(defun org-extra-eldoc-next-variable ()
  "Substitute STR with variable values in BUFF."
  (when (re-search-forward "[‘]\\([^’]+\\)[’]"
                           nil
                           t 1)
    (let* ((str (match-string-no-properties 1))
           (sym (intern str)))
      (when (boundp sym)
        sym))))

(defun org-extra-eldoc-extract-settings (info-str)
  "Extract alist of settings from manual INFO-STR."
  (let ((regex "^[‘]\\([^’]+\\)[’]")
        (result))
    (with-temp-buffer
      (save-excursion
        (insert info-str))
      (while (re-search-forward regex nil t 1)
        (let ((curr (match-string-no-properties 1))
              (start (point))
              (end))
          (setq end (or (save-excursion
                          (when (re-search-forward regex nil t 1)
                            (match-beginning 0)))
                        (line-end-position)))
          (push (cons curr
                      (string-trim
                       (buffer-substring-no-properties start end)))
                result))))
    (nreverse result)))

(defun org-extra-substitute-get-vars (str)
  "Extract and display buffer-local variables from string STR.

Argument STR is a string containing the text to be processed for variable
substitution."
  (let ((buff (current-buffer)))
    (with-temp-buffer
      (save-excursion
        (insert str))
      (while (re-search-forward "[~‘]\\([^’~]+\\)[’~]"
                                nil
                                t 1)
        (let ((str (match-string-no-properties 1))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (let ((sym (intern str)))
            (when (boundp sym)
              (forward-char -1)
              (add-text-properties beg end '(face font-lock-property-name-face))
              (insert ": " (propertize (format "%s"
                                               (buffer-local-value sym buff))
                                       'face
                                       'font-lock-property-name-face))))))
      (buffer-string))))

(defun org-extra-eldoc-info ()
  "Show extra info."
  (or (org-extra-describe-eldoc-setting)
      (when-let* ((el (org-element-at-point))
                  (type (org-element-type el)))
        (if (eq type 'table-row)
            (org-table-field-info nil)
          (let ((pl (car-safe (cdr-safe el))))
            (let ((word (org-extra-get-word))
                  (key (plist-get pl :key)))
              (pcase key
                ("STARTUP"
                 (org-extra-substitute-get-vars
                  (pcase word
                    ("overview" "Top-level headlines only.")
                    ("content" "All headlines.")
                    ("showall" "No folding on any entry.")
                    ("show2levels" "Headline levels 1-2.")
                    ("show3levels" "Headline levels 1-3.")
                    ("show4levels" "Headline levels 1-4.")
                    ("show5levels" "Headline levels 1-5.")
                    ("showeverything" "Show even drawer contents.")
                    ("indent" "Start with Org Indent mode turned on.")
                    ("noindent" "Start with Org Indent mode turned off.")
                    ("num"
                     "~org-startup-numerated~ Start with Org num mode turned on.")
                    ("nonum"
                     "~org-startup-numerated~ Start with Org num mode turned off.")
                    ("align" "~org-startup-align-all-tables~ Align all tables.")
                    ("noalign"
                     "~org-startup-align-all-tables~ Do not align tables on startup.")
                    ("inlineimages"
                     "~org-startup-with-inline-images~ Show inline images.")
                    ("noinlineimages"
                     "~org-startup-with-inline-images~ Do not show inline images on startup.")
                    ("logdone"
                     "~org-log-done~ Record a timestamp when an item is marked as done.")
                    ("lognotedone" "Record timestamp and a note when DONE.")
                    ("nologdone" "Do not record when items are marked as done.")
                    ("logrepeat"
                     "~org-log-repeat~ Record a time when reinstating a repeating item.")
                    ("lognoterepeat"
                     "Record a note when reinstating a repeating item.")
                    ("nologrepeat"
                     "Do not record when reinstating repeating item.")
                    ("lognoteclock-out"
                     "~org-log-note-clock-out~ Record a note when clocking out.")
                    ("nolognoteclock-out"
                     "Do not record a note when clocking out.")
                    ("logreschedule"
                     "Record a timestamp when scheduling time changes.")
                    ("lognotereschedule"
                     "Record a note when scheduling time changes.")
                    ("nologreschedule"
                     "Do not record when a scheduling date changes.")
                    ("logredeadline" "Record a timestamp when deadline changes.")
                    ("lognoteredeadline" "Record a note when deadline changes.")
                    ("nologredeadline"
                     "Do not record when a deadline date changes.")
                    ("logrefile" "Record a timestamp when refiling.")
                    ("lognoterefile" "Record a note when refiling.")
                    ("nologrefile" "Do not record when refiling.")
                    ("hidestars"
                     "~org-hide-leading-stars~ Make all but one of the stars starting a headline invisible.")
                    ("showstars" "Show all stars starting a headline.")
                    ("odd"
                     "~org-odd-levels-only~ Allow only odd outline levels (1, 3, …).")
                    ("oddeven" "Allow all outline levels.")
                    ("customtime"
                     "~org-put-time-stamp-overlays~ ~org-time-stamp-overlay-formats~ Overlay custom time format.")
                    ("constcgs"
                     "‘constants.el’ should use the c-g-s unit system.")
                    ("constSI" "‘constants.el’ should use the SI unit system.")
                    ("fninline"
                     "~org-footnote-define-inline~ Define footnotes inline.")
                    ("fnnoinline" "Define footnotes in separate section.")
                    ("fnlocal"
                     "Define footnotes near first reference, but not inline.")
                    ("fnprompt" "Prompt for footnote labels.")
                    ("fnauto"
                     "Create ‘[fn:1]’-like labels automatically (default).")
                    ("fnconfirm"
                     "Offer automatic label for editing or confirmation.")
                    ("fnadjust" "Automatically renumber and sort footnotes.")
                    ("nofnadjust" "Do not renumber and sort automatically.")
                    ("hideblocks" "Hide all begin/end blocks on startup.")
                    ("nohideblocks" "Do not hide blocks on startup.")
                    ("entitiespretty"
                     "Show entities as UTF-8 characters where possible.")
                    ("entitiesplain" "Leave entities plain.")
                    (_ "Startup options Org uses when first visiting a file"))))
                ("OPTIONS"
                 (or (org-extra-describe-eldoc-setting)
                     "Compact form of export options"))
                ("INFOJS_OPT"
                 (pcase word
                   ("view" "Initial view")
                   ("sdepth"
                    "Maximum headline level as an independent section for info and folding modes")
                   ("toc" "show table of contents")
                   ("tdepth"
                    "depth of the table of contents ~org-export-headline-levels~")
                   ("ftoc" "display toc as a fixed section")
                   ("ltoc" "short contents in each section")
                   ("mouse" "Higlhight color for headings on mouse over")
                   ("buttons" "should view-toggle buttons be everywhere")
                   ("path" "The path to the script")
                   (_ "Options for org-info.js")))
                (_
                 (or
                  (when-let ((descr
                              (cdr
                               (assoc-string word
                                             (append
                                              org-extra-eldoc-short-export-setting
                                              org-extra-eldoc-special-props)))))
                    (concat
                     (propertize (format "%s: (%s): " key type) 'face
                                 'font-lock-keyword-face)
                     (org-extra-substitute-get-vars descr)))
                  (format "%s" type))))))))))

(defun org-extra-eldoc-documentation-function (&rest args)
  "Return breadcrumbs on a headline, ARGS for src block header-line.
Call other documentation functions depending on lang when inside src body."
  (let ((res (or
              (org-extra-eldoc-get-breadcrumb)
              (org-extra-eldoc-get-src-header)
              (when-let ((lang (org-extra-eldoc-get-src-lang)))
                (cond ((string= lang "org")	;Prevent inf-loop for Org src blocks
                       nil)
                      ((or
                        (string= lang "emacs-lisp")
                        (string= lang "elisp"))
                       (apply #'org-extra-eldoc-funcall args))
                      ((or
                        (string= lang "c")
                        ;; https://github.com/nflath/c-eldoc
                        (string= lang "C"))
                       (when
                           (require 'c-eldoc nil t)
                         (c-eldoc-print-current-symbol-info)))
                      ;; https://github.com/zenozeng/css-eldoc
                      ((string= lang "css")
                       (when
                           (require 'css-eldoc nil t)
                         (css-eldoc-function)))
                      ;; https://github.com/zenozeng/php-eldoc
                      ((string= lang "php")
                       (when
                           (require 'php-eldoc nil t)
                         (php-eldoc-function)))
                      ((or
                        (string= lang "go")
                        (string= lang "golang"))
                       (when
                           (require 'go-eldoc nil t)
                         (go-eldoc--documentation-function)))
                      (t
                       (let ((doc-fun (org-extra-eldoc-get-mode-local-documentation-function
                                       lang))
                             (callback (car args)))
                         (when (functionp doc-fun)
                           (if (functionp callback)
                               (funcall doc-fun callback)
                             (funcall doc-fun)))))))
              (org-extra-eldoc-info))))
    res))

(defun org-extra-src-block-params-inner ()
  "If point is inside body of src block return list - (LANGUAGE BEGINNING END)."
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t))
        (unless (save-excursion
                  (beginning-of-line)
                  (re-search-forward
                   "#\\+\\(begin\\)_src\\($\\|[\s\f\t\n\r\v]\\)"
                   (line-end-position)
                   t 1))
          (when (re-search-forward
                 "#\\+\\(begin\\|end\\)_src\\($\\|[\s\f\t\n\r\v]\\)" nil t 1)
            (when-let ((word (match-string-no-properties 1))
                       (end (match-beginning 0)))
              (setq word (downcase word))
              (when (string= word "end")
                (when (re-search-backward
                       "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                       nil t 1)
                  (let ((lang (match-string-no-properties 2)))
                    (forward-line 1)
                    (list lang (point) end)))))))))))

(defun org-extra-eldoc-funcall (callback &rest _ignored)
  "Document function call at point by calling CALLBACK."
  (when (org-extra-src-block-params-inner)
    (when-let* ((sym-info (elisp--fnsym-in-current-sexp))
                (fn-sym (car sym-info))
                (info
                 (when (fboundp fn-sym)
                   (apply #'elisp-get-fnsym-args-string sym-info))))
      (funcall callback info
               :thing fn-sym
               :face (if (functionp fn-sym)
                         'font-lock-function-name-face
                       'font-lock-keyword-face)))))


;;;###autoload
(defun org-extra-back-to-heading ()
  "Move to the heading line of which the present line is a subheading."
  (interactive)
  (if (org-extra-bounds-of-current-block)
      (condition-case nil
          (backward-up-list)
        (error (org-up-heading-safe)))
    (org-up-heading-safe)))

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

\\=(advice-add #\\='org-src-font-lock-fontify-block
            :override #\\='org-extra-src-fontify-advice)."
  (let ((modified (buffer-modified-p)) native-tab-width)
    (remove-text-properties start end '(face nil))
    (let ((lang-mode (org-src-get-lang-mode lang)))
      (when (fboundp lang-mode)
        (let ((string (buffer-substring-no-properties start end))
              (org-buffer (current-buffer)))
          (with-current-buffer
              (get-buffer-create
               (format " *org-src-fontification:%s*" lang-mode))
            (let ((inhibit-modification-hooks nil))
              (erase-buffer)
              ;; Add string and a final space to ensure property change.
              (insert string " "))
            (unless (eq major-mode lang-mode)
              (delay-mode-hooks (funcall lang-mode)))
            (setq native-tab-width tab-width)
            (font-lock-ensure)
            (let ((pos (point-min)) next)
              (while (setq next (next-property-change pos))
              ;; Handle additional properties from font-lock, so as to
              ;; preserve, e.g., composition.
              ;; FIXME: We copy 'font-lock-face property explicitly because
              ;; `font-lock-mode' is not enabled in the buffers starting from
              ;; space and the remapping between 'font-lock-face and 'face
              ;; text properties may thus not be set.  See commit
              ;; 453d634bc.
                (dolist (prop (append '(font-lock-face face)
                                      font-lock-extra-managed-props))
                  (let ((new-prop (get-text-property pos prop)))
                    (when new-prop
                      (if (not (eq prop 'invisible))
                          (put-text-property
                           (+ start (1- pos))
                           (1- (+ start next)) prop new-prop
                           org-buffer)
                           ;; Special case.  `invisible' text property may
                           ;; clash with Org folding.  Do not assign
                           ;; `invisible' text property directly.  Use
                           ;; property alias instead.
                        (let ((invisibility-spec
                               (or
                               ;; ATOM spec.
                                (and (memq new-prop buffer-invisibility-spec)
                                     new-prop)
                                     ;; (ATOM . ELLIPSIS) spec.
                                (assq new-prop buffer-invisibility-spec))))
                          (with-current-buffer org-buffer
                          ;; Add new property alias.
                            (unless (memq 'org-src-invisible
                                          (cdr (assq 'invisible
                                                     char-property-alias-alist)))
                              (setq-local char-property-alias-alist
                                          (cons (cons 'invisible
                                                      (nconc (cdr (assq 'invisible char-property-alias-alist))
                                                             '(org-src-invisible)))
                                                (remove (assq 'invisible char-property-alias-alist)
                                                        char-property-alias-alist))))
                                                        ;; Carry over the invisibility spec, unless
                                                        ;; already present.  Note that there might
                                                        ;; be conflicting invisibility specs from
                                                        ;; different major modes.  We cannot do much
                                                        ;; about this then.
                            (when invisibility-spec
                              (add-to-invisibility-spec invisibility-spec))
                            (put-text-property
                             (+ start (1- pos))
                             (1- (+ start next))
                             'org-src-invisible new-prop
                             org-buffer)))))))
                (setq pos next)))
            (set-buffer-modified-p nil)))))
            ;; Add Org faces.
    (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
      (when (or (facep src-face)
                (listp src-face))
        (font-lock-append-text-property start end 'face src-face))
      (font-lock-append-text-property start end 'face 'org-block))
      ;; Display native tab indentation characters as spaces
    (save-excursion
      (goto-char start)
      (let ((indent-offset
             (if (org-src-preserve-indentation-p) 0
               (+ (progn (backward-char)
                         (let ((buffer-invisibility-spec nil))
                           (current-indentation)))
                  org-edit-src-content-indentation))))
        (while (re-search-forward "^[ ]*\t" end t)
          (let* ((b (and (eq indent-offset (move-to-column indent-offset))
                         (point)))
                 (e (progn (skip-chars-forward "\t")
                           (point)))
                 (s (and b (make-string (* (- e b) native-tab-width) ? ))))
            (when (and b (< b e))
              (add-text-properties b e `(display ,s)))
            (forward-char)))))
            ;; Clear abbreviated link folding.
    (org-fold-region start end nil 'org-link)
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    (set-buffer-modified-p modified)))

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

(defun org-extra-overlay-prompt-region (beg end fn &rest args)
  "Highlight region from BEG to END while invoking FN with ARGS."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn (overlay-put overlay 'face 'diary)
               (apply fn args))
      (delete-overlay overlay))))

(defun org-extra-apply-with-overlay (beg end props fn &rest args)
  "Apply a function with specified overlay properties between two points.

Argument BEG is a position (integer or marker) specifying the beginning of the
region to overlay.
Argument END is a position (integer or marker) specifying the END of the region
to overlay.
Argument PROPS is a list of properties to apply to the overlay.
It can be nil.
Argument FN is a function to be applied within the overlay.
Optional argument ARGS is a list of arguments to pass to the function FN.
It can be empty."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn
          (org-extra-add-overlay-props overlay
                                       (or props
                                           '(face
                                             font-lock-warning-face)))
          (apply fn args))
      (delete-overlay overlay))))

(defun org-extra-add-overlay-props (overlay props)
  "Add properties from PROPS to OVERLAY.

Argument OVERLAY is the overlay to which properties will be added.

Argument PROPS is a property list where even-indexed elements are property names
and the following odd-indexed element is the corresponding value."
  (dotimes (idx (length props))
    (when (eq (logand idx 1) 0)
      (let* ((prop-name (nth idx props))
             (val (plist-get props prop-name)))
        (overlay-put overlay prop-name val)))))

(defun org-extra-call-with-overlays (alist-bounds fn &rest args)
  "Apply FN to ARGS with temporary overlays.

Argument ALIST-BOUNDS is a list of cons cells, where each cons cell contains a
pair of buffer positions (START . END) defining the bounds for an overlay.

Argument FN is the function to be called with the specified arguments.

Remaining arguments ARGS are passed to the function FN."
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
  "Prompt for a language with completion, defaulting to detected language.

Argument CODE is a string containing the source code for language detection."
  (let ((detected-lang
         (when (and
                (require 'language-detection nil t)
                (fboundp 'language-detection-string))
           (alist-get (language-detection-string code)
                      org-extra-languages-alist))))
    (org-extra-read-babel-languages "Language: " nil nil
                                    detected-lang
                                    nil detected-lang)))

;;;###autoload
(defun org-extra-example-blocks-to-org-src (language)
  "Convert example blocks to source blocks in Org mode.

Argument LANGUAGE is a string representing the programming language for the
source code block."
  (interactive (list
                (org-extra-read-babel-languages "Language: " nil nil
                                                nil
                                                nil)))
  (org-with-wide-buffer
   (widen)
   (goto-char (point-max))
   (while (re-search-backward "#\\+\\(begin\\)_example" nil t 1)
     (org-extra-example-block-to-src language "src"))))

;;;###autoload
(defun org-extra-example-block-to-src (&optional language suffix)
  "Convert an Org example block to a source code block.

Optional argument LANGUAGE is a string specifying the programming language for
the source block.

Optional argument SUFFIX is a string appended to the \"#+begin_\" line of the
source block; it defaults to \"src\" if LANGUAGE is provided, otherwise it is
prompted from the user."
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
              (or suffix
                  (if language
                      "src"
                    (org-extra-call-with-overlays
                     alist-bounds
                     (lambda ()
                       (completing-read
                        "Replace with"
                        (mapcar #'cdr
                                org-structure-template-alist)))))))
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
  "Convert example blocks to source blocks in Org mode.

Optional argument LANGUAGE is a string specifying the programming language for
the source block.

Optional argument SUFFIX is a string appended to the source block's header
arguments."
  (interactive)
  (org-with-wide-buffer
   (widen)
   (goto-char (point-max))
   (while (re-search-backward "#\\+\\(begin\\)_example" nil t 1)
     (org-extra-example-block-to-src language suffix))))

(defun org-extra-flatten-alists (items &optional acc)
  "Flatten nested lists in ITEMS into a single list.

Argument ITEMS is a list of items to flatten.

Optional argument ACC is an accumulator for the result, initially nil."
  (cond ((and items
              (proper-list-p items))
         (seq-mapcat #'org-extra-flatten-alists
                     items))
        ((and
          items
          (consp items)
          (not (listp (cdr items))))
         (push items acc))
        (t acc)))

(defun org-extra-get-code-name (code &optional src-mode)
  "Extract CODE block names from `code' using SRC-MODE.

Argument CODE is a string containing the source code to be analyzed.

Optional argument SRC-MODE is a major mode function used to set the buffer's
mode for parsing CODE."
  (let ((re (org-babel-noweb-wrap)))
    (with-temp-buffer
      (require 'imenu)
      (insert code)
      (while (re-search-backward re nil t 1)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (delete-region beg end)))
      (funcall src-mode)
      (let ((result
             (ignore-errors
               (mapcar #'car (org-extra-flatten-alists
                             (funcall
                              imenu-create-index-function))))))
        (pcase src-mode
          ('emacs-lisp-mode
           (let ((sexp))
             (goto-char (point-min))
             (while
                 (setq sexp (ignore-errors (read (current-buffer))))
               (when (listp sexp)
                 (pcase-let
                     ((`(,type ,name . ,_rest) sexp))
                   (cond ((and name (symbolp name)
                               (not (eq name t))
                               (memq type
                                     '(define-skeleton
                                        ert-deftest
                                        define-widget
                                        easy-mmode-define-minor-mode
                                        defclass cl-defstruct
                                        defvar defconst
                                        defvar defface
                                        defcustom defgroup deftheme
                                        defun defmacro defsubst
                                        defalias
                                        defhydra transient-define-prefix
                                        transient-define-suffix
                                        transient-define-argument
                                        transient-define-infix
                                        cl-defun cl-defsubst
                                        cl-defmacro
                                        cl-defgeneric
                                        cl-defmethod define-minor-mode
                                        define-derived-mode
                                        define-generic-mode
                                        define-compilation-mode
                                        easy-mmode-define-minor-mode))
                               (let ((str (symbol-name
                                           name)))
                                 (unless (member str
                                                 result)
                                   (push str
                                         result))))))))))))
        result))))

;;;###autoload
(defun org-extra-add-names-to-src-blocks ()
  "Add names to unnamed Org source blocks."
  (interactive)
  (require 'org-extra-complete nil t)
  (widen)
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (when (org-babel-active-location-p)
        (let ((full-block (match-string 0))
              (beg-block (match-beginning 0))
              (end-block (match-end 0))
              (lang (match-string 2))
              (beg-lang (match-beginning 2))
              (end-lang (match-end 2))
              (switches (match-string 3))
              (beg-switches (match-beginning 3))
              (end-switches (match-end 3))
              (header-args (match-string 4))
              (beg-header-args (match-beginning 4))
              (end-header-args (match-end 4))
              (body (match-string 5))
              (beg-body (match-beginning 5))
              (end-body (match-end 5)))
          ;; Silence byte-compiler in case `body' doesn't use all
          ;; those variables.
          (ignore full-block beg-block end-block lang
                  beg-lang end-lang switches beg-switches
                  end-switches header-args beg-header-args
                  end-header-args body beg-body end-body)
          (unless (nth 4 (org-babel-get-src-block-info 'no-eval))
            (save-excursion
              (goto-char beg-block)
              (recenter-top-bottom)
              (let* ((choices (org-extra-get-code-name
                               body
                               (org-src-get-lang-mode lang)))
                     (name (org-extra-apply-with-overlay
                            (line-beginning-position)
                            (line-end-position)
                            `(before-string
                              ,(propertize (concat "#+name: " (or
                                                               (car
                                                                choices)
                                                               "")
                                                   "\n")
                                           'face 'font-lock-warning-face))
                            #'completing-read
                            "#+name: "
                            choices)))
                (unless (string-empty-p name)
                  (insert "#+name: " name "\n")))))
          (goto-char end-block))))))

(defun org-extra-get-html-head ()
  "Extract and format CSS content for HTML head from a file."
  (let ((content (with-temp-buffer
                   (erase-buffer)
                   (insert-file-contents (expand-file-name
                                          "org-html-head.css"
                                          org-extra-preview-data-root))
                   (buffer-string))))
    (format "<style type=\"text/css\">\n%s</style>" content)))

(defun org-extra-get-html-scripts ()
  "Insert JavaScript content into an HTML script tag."
  (let ((content (with-temp-buffer
                   (erase-buffer)
                   (insert-file-contents (expand-file-name
                                          "org-html-scripts.js"
                                          org-extra-preview-data-root))
                   (buffer-string))))
    (format "<script type=\"text/javascript\">\n%s</script>" content)))

;;;###autoload
(defun org-extra-narrow-to-block-content ()
  "Narrow view to the content of the current Org block."
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
  "Create a toggle DESCRIPTION with alignment and optional labels.

Argument DESCRIPTION is a string that represents the description of the toggle.

Argument VALUE is a boolean indicating the current state of the toggle.

Optional argument ON-LABEL is a string used when VALUE is non-nil. It defaults
to \"+\".

Optional argument OFF-LABEL is a string used when VALUE is nil. It defaults to
\"-\".

Optional argument LEFT-SEPARATOR is a string placed before the ON-LABEL or
OFF-LABEL. It has no default value.

Optional argument RIGHT-SEPARATOR is a string placed after the ON-LABEL or
OFF-LABEL. It has no default value."
  (let* ((description (or description ""))
         (align (apply #'max (list (+ 5 (length description))
                                   30))))
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


;;;###autoload
(defun org-extra-reload ()
  "Reload Org mode with optional setup."
  (interactive)
  (org-reload t))

;;;###autoload (autoload 'org-extra-refresh-reload-menu "org-extra" nil t)
(transient-define-prefix org-extra-refresh-reload-menu ()
  "Reload Org configurations with refresh options."
  [("r" "Refresh setup current buffer" org-mode-restart)
   ("e" "Reload Org (after update) (C-c C-x !)" org-reload)
   ("l" "Reload Org uncompiled" org-extra-reload)])

;;;###autoload (autoload 'org-extra-customize-menu "org-extra" nil t)
(transient-define-prefix org-extra-customize-menu ()
  "Define a transient menu for Org customization."
  [("b" "Browse Org Group" org-customize)])

;;;###autoload (autoload 'org-extra-documentation-menu "org-extra" nil t)
(transient-define-prefix org-extra-documentation-menu ()
  "Display Org mode version, info docs, or news."
  [("s" "Show Version" org-version)
   ("i" "Info Documentation" org-info)
   ("b" "Browse Org News" org-browse-news)])

;;;###autoload (autoload 'org-extra-latex-menu "org-extra" nil t)
(transient-define-prefix org-extra-latex-menu ()
  "Toggle Org CDLaTeX mode and insert citations."
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

;;;###autoload (autoload 'org-extra-special-views-current-file-menu "org-extra" nil t)
(transient-define-prefix org-extra-special-views-current-file-menu ()
  "Display custom Org mode views for the current file."
  [("t" "TODO Tree" org-show-todo-tree)
   ("c" "Check Deadlines" org-check-deadlines)
   ("a" "Tags/Property tree (C-c \\)" org-match-sparse-tree)])

;;;###autoload (autoload 'org-extra-file-list-for-agenda-menu "org-extra" nil t)
(transient-define-prefix org-extra-file-list-for-agenda-menu ()
  "Define transient menu for managing Org agenda files."
  [("e" "Edit File List" org-edit-agenda-file-list)
   ("a" "Add/Move Current File to Front of List (C-c [)"
    org-agenda-file-to-front)
   ("r" "Remove Current File from List (C-c ])" org-remove-file)
   ("c" "Cycle through agenda files" org-cycle-agenda-files :transient t)
   ("o" "Occur in all agenda files" org-occur-in-agenda-files)])

;;;###autoload
(defun org-extra-logging-record-done-time ()
  "Toggle recording of timestamps for completed tasks."
  (interactive)
  (progn (setq org-log-done (not org-log-done))
         (message "Switching to %s will %s record a timestamp"
                  (car org-done-keywords)
                  (if org-log-done "automatically" "not"))))

;;;###autoload
(defun org-extra-clock-in ()
  "Clock in with a universal argument."
  (interactive)
  (org-clock-in '(4)))

;;;###autoload
(defun org-extra-clock-in-mark ()
  "Mark the current heading for clock-in with a specific effort."
  (interactive)
  (org-clock-in '(16)))

;;;###autoload (autoload 'org-extra-logging-work-menu "org-extra" nil t)
(transient-define-prefix org-extra-logging-work-menu ()
  "Display a menu for clocking and logging tasks."
  :refresh-suffixes t
  [("i" "Clock in" org-clock-in)
   ("s" "Switch task" org-extra-clock-in)
   ("o" "Clock out" org-clock-out)
   ""
   ("C" "Clock cancel" org-clock-cancel)
   ("m" "Mark as default task" org-clock-mark-default-task)
   ("k" "Clock in, mark as default" org-extra-clock-in-mark)
   ("j" "Goto running clock" org-clock-goto)
   ""
   ("d" "Display times" org-clock-display)
   ("e" org-extra-logging-record-done-time
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Record DONE time"
                                              org-log-done "+" ""
                                              "[" "]"))
    :transient t)])

;;;###autoload (autoload 'org-extra-dates-and-scheduling-menu "org-extra" nil t)
(transient-define-prefix org-extra-dates-and-scheduling-menu ()
  "Display Org mode date and scheduling options menu."
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

;;;###autoload (autoload 'org-extra-change-date-menu "org-extra" nil t)
(transient-define-prefix org-extra-change-date-menu ()
  "Define a transient menu for changing dates in Org mode."
    :refresh-suffixes t
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

;;;###autoload (autoload 'org-extra-tags-and-properties-menu "org-extra" nil t)
(transient-define-prefix org-extra-tags-and-properties-menu ()
  "Define a transient menu for Org mode tag and property actions."
  :refresh-suffixes t
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
   ("P" "Set property and value" org-set-property-and-value)
   ("o" org-extra-columns-toggle :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Column view of properties"
                                              org-columns-current-fmt "+"
                                              "" "[" "]")))
   ("i" "Insert Column View DBlock" org-columns-insert-dblock)
   ("C-u" "dblock-update" org-dblock-update)])

;;;###autoload
(defun org-extra-customize-org-enforce-todo-dependencies ()
  "Customize the enforcement of TODO dependencies."
  (interactive)
  (customize-variable 'org-enforce-todo-dependencies))

;;;###autoload
(defun org-extra-customize-feed ()
  "Customize the `org-feed-alist' variable."
  (interactive)
  (customize-variable 'org-feed-alist))

;;;###autoload (autoload 'org-extra-todo-lists-menu "org-extra" nil t)
(transient-define-prefix org-extra-todo-lists-menu ()
  "Display a menu for extra Org mode TODO list actions."
  :refresh-suffixes t
  [("t" "TODO/DONE/- (C-c C-t)" org-todo :transient t)
   ("s" "Select keyword" org-extra-select-keyword-menu)
   ("h" "Show TODO Tree" org-show-todo-tree :transient t)
   ("g" "Global TODO list (C-c k t)" org-todo-list)
   ("e" org-extra-customize-org-enforce-todo-dependencies
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
   ("c" "Customize feeds" org-extra-customize-feed)])

;;;###autoload (autoload 'org-extra-select-keyword-menu "org-extra" nil t)
(transient-define-prefix org-extra-select-keyword-menu ()
  "Navigate Org keywords with transient menu options."
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

;;;###autoload (autoload 'org-extra-hyperlinks-menu "org-extra" nil t)
(transient-define-prefix org-extra-hyperlinks-menu ()
  "Define a menu for Org hyperlink actions."
  :refresh-suffixes t
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
                                               org-link-descriptive)
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



;;;###autoload
(defun org-extra-footnote ()
  "Invoke `org-footnote-action' with extra argument."
  (interactive)
  (org-footnote-action t))

;;;###autoload (autoload 'org-extra-editing-menu "org-extra" nil t)
(transient-define-prefix org-extra-editing-menu ()
  "Display a menu for extra Org editing actions."
  [("e" "Emphasis... (C-c C-x C-f)" org-emphasize)
   ("a" "Add block structure (C-c C-,)" org-insert-structure-template)
   ("d" "Edit Source Example (C-c ')" org-edit-special)
   ("f" "Footnote new/jump (C-c C-x f)" org-footnote-action)
   ("o" "Footnote extra" org-extra-footnote)])

;;;###autoload (autoload 'org-extra-edit-structure-menu "org-extra" nil t)
(transient-define-prefix org-extra-edit-structure-menu ()
  "Display menu for editing Org structure."
  :refresh-suffixes t
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

;;;###autoload (autoload 'org-extra-navigate-headings-menu "org-extra" nil t)
(transient-define-prefix org-extra-navigate-headings-menu ()
  "Navigate Org headings with transient menu commands."
  :transient-suffix #'transient--do-call
  :refresh-suffixes t
  :transient-non-suffix #'transient--do-stay
  [("u" "Up" outline-up-heading
    :transient t)
   ("n" "Next" outline-next-visible-heading
    :transient t)
   ("p" "Previous" outline-previous-visible-heading
    :transient t)
   ("e" "Next Same Level" outline-forward-same-level
    :transient t)
   ("r" "Previous Same Level" outline-backward-same-level
    :transient t)
   ("j" "Jump" org-goto)])

;;;###autoload (autoload 'org-extra-show-hide-menu "org-extra" nil t)
(transient-define-prefix org-extra-show-hide-menu ()
  "Toggle visibility options for Org mode elements."
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

;;;###autoload
(defun org-extra-remove-all-results ()
  "Add extra Babel menu options for result removal."
  (interactive)
  (org-babel-remove-result-one-or-many t))

;;;###autoload (autoload 'org-extra-menu-babel-transient "org-extra" nil t)
(transient-define-prefix org-extra-menu-babel-transient ()
  "Provide Babel source block actions in a transient menu."
  :transient-suffix #'transient--do-call
  :refresh-suffixes t
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
    ("D" "remove all" org-extra-remove-all-results)
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

;;;###autoload
(defun org-extra-table-insert-row-below ()
  "Insert a new row below the current one in an Org table."
  (interactive)
  (org-table-insert-row t))


;;;###autoload (autoload 'org-extra-table-plot-menu "org-extra" nil t)
(transient-define-prefix org-extra-table-plot-menu ()
  "Transient menu for Plot commands."
  ["table -> Plot"
   [("a" orgtbl-ascii-plot :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Ascii plot"
         (propertize "Ascii plot" 'face 'transient-inapt-suffix))))
    ("g" org-plot/gnuplot :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Gnuplot"
         (propertize "Gnuplot" 'face 'transient-inapt-suffix))))]])

;;;###autoload (autoload 'org-extra-table-calculate-menu "org-extra" nil t)
(transient-define-prefix org-extra-table-calculate-menu ()
  "Transient menu for Calculate commands."
  ["table -> Calculate"
   [("=" org-table-eval-formula :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Set Column Formula"
         (propertize "Set Column Formula" 'face 'transient-inapt-suffix))))
    ("'" org-edit-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Edit Formulas"
         (propertize "Edit Formulas" 'face 'transient-inapt-suffix))))
    ""
    ("r" org-table-recalculate :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Recalculate line"
         (propertize "Recalculate line" 'face 'transient-inapt-suffix))))
    ""
    ("#" org-table-rotate-recalc-marks
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Toggle Recalculate Mark"
         (propertize "Toggle Recalculate Mark" 'face 'transient-inapt-suffix)))
     :transient t)
    ""
    ("+" org-table-sum :description
     (lambda ()
       (if
           (ignore-errors
             (or
              (org-at-table-p)
              (org-region-active-p)))
           "Sum Column/Rectangle"
         (propertize "Sum Column/Rectangle" 'face 'transient-inapt-suffix))))
    ("w" org-table-current-column :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Which Column?"
         (propertize "Which Column?" 'face 'transient-inapt-suffix))))]])

;;;###autoload (autoload 'org-extra-table-rectangle-menu "org-extra" nil t)
(transient-define-prefix org-extra-table-rectangle-menu ()
  "Transient menu for Rectangle commands."
  ["table -> Rectangle"
   [("w" org-copy-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Copy Rectangle"
         (propertize "Copy Rectangle" 'face 'transient-inapt-suffix))))
    ("c" org-cut-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Cut Rectangle"
         (propertize "Cut Rectangle" 'face 'transient-inapt-suffix))))
    ("y" org-paste-special
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Paste Rectangle"
         (propertize "Paste Rectangle" 'face 'transient-inapt-suffix)))
     :transient t)
    ("f" org-table-wrap-region :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Fill Rectangle"
         (propertize "Fill Rectangle" 'face 'transient-inapt-suffix))))]])


(defun org-extra-post-command-transient ()
  "Manage transient state after Org mode commands."
  (cond ((or (not transient--window)
             (memq this-command '(transient-quit-all transient-quit-one)))
         (remove-hook 'post-command-hook #'org-extra-post-command-transient t))
        ((or (memq last-command '(org-table-edit-field))
             (memq this-command '(org-table-edit-field)))
         (transient--emergency-exit))
        ((not (memq this-command '(org-extra-table-rectangle-menu
                                   org-extra-table-plot-menu
                                   org-extra-table-calculate-menu)))
         (and transient-current-command
              (transient-setup transient-current-command)))))


(defun org-extra-get-current-column-name ()
  "Retrieve the name of the current column in an Org table."
  (when (org-at-table-p)
    (when-let ((col (org-table-current-column)))
      (save-excursion
        (goto-char (org-table-begin))
        (when (save-excursion
                (forward-line 1)
                (org-at-table-hline-p))
          (org-table-goto-column col)
          (org-table-get (org-table-current-line)
                         (org-table-current-column)))))))

(defun org-extra-convert-table ()
  "Convert selected region to a table or create a new table."
  (cond ((org-region-active-p)
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
            ((or ?n ?q) nil))))
        (t (org-table-create (format "%sx%s"
                                     (read-number "Columns: " 2)
                                     (read-number "Rows: " 4))))))



(transient-define-prefix org-extra-create-table-menu ()
  "Define a menu for creating Org tables with options."
  ["Create Table"
   ("c" "Columns" "--columns="
    :class transient-option
    :allow-empty nil
    :always-read t
    :init-value
    (lambda (obj)
      (oset obj value
            (car (org-split-string org-table-default-size " *x *"))))
    :reader transient-read-number-N+)
   ("r" "Rows" "--rows="
    :class transient-option
    :allow-empty nil
    :always-read t
    :init-value
    (lambda (obj)
      (oset obj value
            (nth 1 (org-split-string org-table-default-size " *x *")))))]
  ["Actions"
   ("RET" "Run"
    (lambda ()
      (interactive)
      (let* ((args
              (transient-args transient-current-command))
             (size (format "%sx%s"
                           (or (transient-arg-value "--columns=" args)
                               (car (org-split-string org-table-default-size
                                                      " *x *")))
                           (or (transient-arg-value "--rows=" args)
                               (nth 1 (org-split-string org-table-default-size
                                                        " *x *"))))))
        (print size))))])

;;;###autoload (autoload 'org-extra-menu-org-table-transient "org-extra" nil t)
(transient-define-prefix org-extra-menu-org-table-transient ()
  "Add table manipulation options to Org mode."
  :transient-non-suffix #'transient--do-stay
  :transient-suffix #'transient--do-call
  :refresh-suffixes t
  [:if org-at-table-p
   [:description (lambda ()
                   (concat
                    (propertize
                     "Column "
                     'face
                     'transient-heading)
                    (if-let ((column (or (org-extra-get-current-column-name)
                                         (org-table-current-column))))
                        (propertize (format "%s" column) 'face 'transient-value)
                      "")))
    :setup-children (lambda (&rest _)
                      (mapcar
                       (apply-partially #'transient-parse-suffix
                                        transient--prefix)
                       (append
                        (list ""
                              '("<left>"  "Move Column Left"
                                org-table-move-column-left
                                :transient t
                                :inapt-if-not (lambda ()
                                                (and (org-at-table-p)
                                                 (let
                                                     ((col
                                                       (org-table-current-column)))
                                                   (and col
                                                    (> col 1))))))
                              '("<right>" "Move Column Right"
                                org-table-move-column-right
                                :inapt-if-not (lambda ()
                                                (and (org-at-table-p)
                                                 (let
                                                     ((col
                                                       (org-table-current-column)))
                                                   (and col
                                                    (>= col 1))))))
                              '("d" "Delete Column"
                                org-shiftmetaleft :transient t)
                              '("w"  "Shrink Column"
                                org-table-toggle-column-width
                                :transient t))
                        (list
                         ""
                         "Field"
                         '("RET" "Next Row" org-return :transient t)
                         '("TAB" "Next Field" org-cycle)
                         '("<backtab>"  "Previous Field"
                           org-shifttab
                           :transient t)
                         ""
                         '("b"  org-table-blank-field
                           :description (lambda ()
                                          (concat "Blank Field "
                                           (or
                                            (ignore-errors
                                              (truncate-string-to-width
                                               (string-trim
                                                (org-table-get-field
                                                 (org-table-current-column)))
                                               20
                                               nil
                                               (string-to-char "\s")
                                               t))
                                            ""))))
                         '("`" org-table-edit-field :description
                           (lambda ()
                             (if
                                 (ignore-errors
                                   (org-at-table-p))
                                 "Edit Field"
                               (propertize "Edit Field" 'face
                                'transient-inapt-suffix))))
                         '("V" org-table-copy-down
                           :description
                           (lambda ()
                             (if
                                 (ignore-errors
                                   (org-at-table-p))
                                 "Copy Field from Above"
                               (propertize "Copy Field from Above" 'face
                                'transient-inapt-suffix))))
                         ""
                         "Sort")
                        (mapcan
                         (lambda (it)
                           (let* ((key (substring-no-properties it 0 1))
                                  (upcased-key (upcase key)))
                             (list (list key it
                                         `(lambda (&optional with-case)
                                            (interactive
                                             (list current-prefix-arg))
                                            (let ((char ,(string-to-char
                                                          key)))
                                             (funcall
                                              #'org-table-sort-lines
                                              with-case
                                              char))))
                                   (list upcased-key
                                         (format "%s (reversed)" (capitalize
                                                                  it))
                                         `(lambda (&optional with-case)
                                            (interactive
                                             (list
                                              current-prefix-arg))
                                            (let ((char ,(string-to-char
                                                          upcased-key)))
                                             (funcall
                                              #'org-table-sort-lines
                                              with-case
                                              char)))))))
                         (list "alphabetic" "numeric" "time" "func")))))]
   [:description (lambda ()
                   (format "Row %s" (org-table-current-line)))
    ("M-<up>" "Move Row Up" org-metaup)
    ("M-<down>" "Move Row Down" org-metadown)
    ("D" "Delete Row" org-shiftmetaup)
    ("s" "Sort lines in region" org-table-sort-lines)
    ""
    "Insert"
    ("i" "Column" org-shiftmetaright
     :transient t)
    ("h" "Header line" org-ctrl-c-minus :transient t)
    ("C-<return>"  "Row" org-shiftmetadown)
    ""
    "Toggle"
    ("}" org-table-toggle-coordinate-overlays
     :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Show Col/Row Numbers"
                                               (bound-and-true-p
                                                org-table-overlay-coordinates)
                                               "+" " " " [" "] ")))
    ("{" org-table-toggle-formula-debugger
     :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Debug Formulas"
                                               (bound-and-true-p
                                                org-table-formula-debug)
                                               "+" " " " [" "] ")))
    ("C" "Calculate" org-extra-table-calculate-menu)
    ("R" "Rectangle" org-extra-table-rectangle-menu
     :transient nil)
    ""
    ("e" org-table-create :description
     (lambda ()
       (if
           (ignore-errors
             (not
              (org-at-table-p)))
           "Create"
         (propertize "Create" 'face 'transient-inapt-suffix))))
    ("O" org-table-convert-region :description
     (lambda ()
       (if
           (ignore-errors
             (not
              (org-at-table-p 'any)))
           "Convert Region"
         (propertize "Convert Region" 'face 'transient-inapt-suffix))))
    ("E" org-table-export :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Export to File"
         (propertize "Export to File" 'face 'transient-inapt-suffix))))
    ("T" "Transpose table" org-table-transpose-table-at-point)
    ""
    ("~" "Create/Convert from/to table.el" org-table-create-with-table.el)
    ("P" "Plot" org-extra-table-plot-menu :transient nil)
    ""
    ("q" "Quit" transient-quit-all)]]
  [:if-not org-at-table-p
   [("I" org-table-import :description
     (lambda ()
       (if
           (ignore-errors
             (not
              (org-at-table-p)))
           "Import from File"
         (propertize "Import from File" 'face 'transient-inapt-suffix))))]]
  (interactive)
  (require 'org)
  (require 'org-table)
  (if
      (when (fboundp 'org-at-table-p)
        (org-at-table-p))
      (transient-setup #'org-extra-menu-org-table-transient)
    (if
        (when (fboundp 'org-region-active-p)
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

;;;###autoload
(defun org-extra-info-timers ()
  "Display \"Timers\" info from Org mode."
  (interactive)
  (org-info "Timers"))

;;;###autoload
(defun org-extra-info-clock-commands ()
  "Display clocking commands in Org mode."
  (interactive)
  (org-info "Clocking commands"))



;;;###autoload
(defun org-extra-agenda-archives-files ()
  "Toggle archive mode for Org agenda files."
  (interactive)
  (require 'org-agenda)
  (when (fboundp 'org-agenda-archives-mode)
    (org-agenda-archives-mode 'files)))

;;;###autoload (autoload 'org-extra-agenda-transient "org-extra" nil t)
(transient-define-prefix org-extra-agenda-transient ()
  "Toggle agenda views and actions with a transient interface."
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


(defun org-extra-get-most-long-table-size ()
  "Calculate the width of the longest Org table."
  (let ((len))
    (org-table-map-tables
     (lambda ()
       (let ((column-len (progn (goto-char (line-end-position))
                                (current-column))))
         (if (not len)
             (setq len column-len)
           (if (> column-len len)
               (setq len column-len))))))
    len))

(defun org-extra--checkbox-toggable-p ()
  "Check if a checkbox can be toggled."
  (or (org-at-radio-list-p)
      (org-at-item-p)
      (save-excursion
        (cond ((org-region-active-p)
               (let ((limit (region-end)))
                 (goto-char (region-beginning))
                 (org-list-search-forward (org-item-beginning-re)
                                          limit t)))
              ((org-at-heading-p)
               (let ((limit (save-excursion
                              (outline-next-heading)
                              (point))))
                 (org-end-of-meta-data t)
                 (org-list-search-forward (org-item-beginning-re)
                                          limit t)))
              ((org-at-item-p))))))

;;;###autoload (autoload 'org-extra-toggle-menu "org-extra" nil t)
(transient-define-prefix org-extra-toggle-menu ()
  "Toggle various Org mode elements' visibility."
  :transient-suffix #'transient--do-call
  :transient-non-suffix #'transient--do-stay
  :refresh-suffixes t
  [["Toggle visibility"
    ("i" "Images" org-toggle-inline-images)
    ("I" "Redisplay Inline images" org-redisplay-inline-images)
    ("b" "Block" org-fold-hide-block-toggle)
    ("d" "Drawer" org-fold-hide-drawer-toggle)
    ("C" "Custom properties" org-toggle-custom-properties-visibility)
    ("l" org-toggle-link-display :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Links"
                                               org-link-descriptive)))
    ("E"  org-toggle-pretty-entities
     :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Entities"
                                               org-pretty-entities
                                               "UTF8"
                                               "plain text")))]
   ["Toggle buttons"
    ("c" "Comment" org-toggle-comment)
    ("x" "Checkbox" org-toggle-checkbox :inapt-if-not
     org-extra--checkbox-toggable-p)
    ("r" "Radio" org-toggle-radio-button :inapt-if-not org-at-item-p)
    ("a" "Archive tag" org-toggle-archive-tag :transient nil)]]
  ["Toggle headings"
   ("H" "Headings => normal lines => list" org-toggle-item)
   ("h" "Headings => normal text" org-toggle-heading)]
  [("w" "Fixed-width markup" org-toggle-fixed-width)
   ("g" "Group tags" org-toggle-tags-groups)
   ("o" "Ordered property" org-toggle-ordered-property)
   ("t" "Timestamp" org-toggle-timestamp-type)
   ("e" "Custom time stamp formats" org-toggle-time-stamp-overlays)
   ("u" "Debugging flags for ‘org-gcal’" org-gcal-toggle-debug)])

;;;###autoload (autoload 'org-extra-menu-clock "org-extra" nil t)
(transient-define-prefix org-extra-menu-clock ()
  "Define a transient menu for Org clock and timer actions."
  ["Clock in/out"
   [("i" "Clock in" org-clock-in)
    ("c" "Continiue" org-clock-in-last)
    ("o" "Out" org-clock-out)
    ("q" "Cancel" org-clock-cancel)
    ("e" "Change effort" org-clock-modify-effort-estimate)
    ("C-z" "resolve-clocks" org-resolve-clocks)]
   ["Summary"
    ("g" "Goto " org-clock-goto)
    ("d" "Show subtree timers" org-clock-display)
    ("r" "Report" org-clock-report)]
   ["Help"
    ("z" "Info about Timers" org-extra-info-timers)
    ("?" "Show info" org-extra-info-clock-commands)]]
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

(defun org-extra-columns-toggle ()
  "Toggle display of columns in Org mode."
  (interactive)
  (if org-columns-current-fmt
      (org-columns-quit)
    (org-columns)))

;;;###autoload (autoload 'org-extra-menu-archive "org-extra" nil t)
(transient-define-prefix org-extra-menu-archive ()
  "Display menu for archiving Org mode subtrees."
  :refresh-suffixes t
  [("a" org-archive-subtree-default
    :description (lambda ()
                   (format "Archive (default method %s)"
                           org-archive-default-command))
    :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ""
   ("m" "Move Subtree to Archive file" org-archive-subtree :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))
   ("t" "Toggle ARCHIVE tag" org-toggle-archive-tag :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p)))
    :transient t)
   ("o" "Move subtree to Archive sibling"
    org-archive-to-archive-sibling :inapt-if-not
    (lambda ()
      (ignore-errors
        (org-in-subtree-not-table-p))))])




(transient-define-prefix org-extra-menu-customize ()
   "Transient menu for Customize commands." :refresh-suffixes t
  ["org -> Customize"
   [("b" "Browse Org Group" org-customize)
    ""
    ("e" "Expand This Menu" org-create-customize-menu)]])


(transient-define-prefix org-extra-menu-documentation ()
   "Transient menu for Documentation commands." :refresh-suffixes t
  ["org -> Documentation"
   [("s" "Show Version" org-version)
    ("i" "Info Documentation" org-info)
    ("b" "Browse Org News" org-browse-news)]])


(transient-define-prefix org-extra-menu-latex ()
  "Transient menu for LaTeX commands."
  :refresh-suffixes t
  ["org -> LaTeX"
   [("o" org-cdlatex-mode
     :description
     (lambda ()
       (let ((descr
              (org-extra--bar-make-toggle-description "Org CDLaTeX mode"
                                                      org-cdlatex-mode "+" " " "[" "]")))
         (if
             (ignore-errors
               (require 'cdlatex nil t))
             descr
           (propertize descr 'face 'transient-inapt-suffix))))
     :transient t)
    nil nil
    ("m" org-cdlatex-math-modify :description
     (lambda ()
       (if
           (ignore-errors
             (org-inside-LaTeX-fragment-p))
           "Modify math symbol"
         (propertize "Modify math symbol" 'face 'transient-inapt-suffix))))
    ("i" "Insert citation" org-reftex-citation)]])


(transient-define-prefix org-extra-menu-specialviewscurrentfile ()
   "Transient menu for Special views current file commands." :refresh-suffixes t
  ["org -> Special views current file"
   [("t" "TODO Tree" org-show-todo-tree)
    ("c" "Check Deadlines" org-check-deadlines)
    ("\\" "Tags/Property tree" org-match-sparse-tree)]])


(transient-define-prefix org-extra-menu-filelistforagenda ()
  "Transient menu for File List for Agenda commands."
  :refresh-suffixes t
  ["org -> File List for Agenda"
   [("e" "Edit File List" org-edit-agenda-file-list)
    ("[" "Add/Move Current File to Front of List" org-agenda-file-to-front)
    ("]" "Remove Current File from List" org-remove-file)
    ("c" "Cycle through agenda files" org-cycle-agenda-files :transient t)
    ("o" "Occur in all agenda files" org-occur-in-agenda-files)]])



(transient-define-prefix org-extra-menu-datesandscheduling ()
   "Transient menu for Dates and Scheduling commands." :refresh-suffixes t
  ["org -> Dates and Scheduling"
   [("." org-timestamp :description
     (lambda () 			 (if
           (ignore-errors
             (not
              (org-before-first-heading-p)))
           "Timestamp"
         (propertize "Timestamp" 'face 'transient-inapt-suffix))))
    ("!" org-timestamp-inactive :description
     (lambda () 			 (if
           (ignore-errors
             (not
              (org-before-first-heading-p)))
           "Timestamp (inactive)"
         (propertize "Timestamp (inactive)" 'face 'transient-inapt-suffix))))
    ("c" "Change Date" org-extra-menu-changedate)
    ("y" "Compute Time Range" org-evaluate-time-range)
    ("s" org-schedule :description
     (lambda () 			 (if
           (ignore-errors
             (not
              (org-before-first-heading-p)))
           "Schedule Item"
         (propertize "Schedule Item" 'face 'transient-inapt-suffix))))
    ("d" org-deadline :description
     (lambda () 			 (if
           (ignore-errors
             (not
              (org-before-first-heading-p)))
           "Deadline"
         (propertize "Deadline" 'face 'transient-inapt-suffix))))
    ""
    ("C" org-toggle-timestamp-overlays :description
     (lambda () 			 (org-extra--bar-make-toggle-description "Custom time format" org-display-custom-times "*" " " "(" ")"))
     :transient t)
    ""
    (">" "Goto Calendar" org-goto-calendar)
    ("<" "Date from Calendar" org-date-from-calendar)
    ""
    ("S" "Start/Restart Timer" org-timer-start)
    ("p" "Pause/Continue Timer" org-timer-pause-or-continue)
    ("t" "Stop Timer" org-timer-pause-or-continue)
    ("i" "Insert Timer String" org-timer)
    ("I" "Insert Timer Item" org-timer-item)]])


(transient-define-prefix org-extra-menu-changedate ()
   "Transient menu for Change Date commands." :refresh-suffixes t
  ["org -> Dates and Scheduling -> Change Date"
   [("d" org-shiftright :description
     (lambda () 			 (if
           (ignore-errors
             (org-at-timestamp-p 'lax))
           "1 Day Later"
         (propertize "1 Day Later" 'face 'transient-inapt-suffix))))
    ("D" org-shiftleft :description
     (lambda () 			 (if
           (ignore-errors
             (org-at-timestamp-p 'lax))
           "1 Day Earlier"
         (propertize "1 Day Earlier" 'face 'transient-inapt-suffix))))
    ("l" org-shiftup :description
     (lambda () 			 (if
           (ignore-errors
             (org-at-timestamp-p 'lax))
           "1 ... Later"
         (propertize "1 ... Later" 'face 'transient-inapt-suffix))))
    ("e" org-shiftdown :description
     (lambda () 			 (if
           (ignore-errors
             (org-at-timestamp-p 'lax))
           "1 ... Earlier"
         (propertize "1 ... Earlier" 'face 'transient-inapt-suffix))))]])


(transient-define-prefix org-extra-menu-tagsandproperties ()
   "Transient menu for TAGS and Properties commands." :refresh-suffixes t
  ["org -> TAGS and Properties"
   [("q" org-set-tags-command :description
     (lambda () 			 (if
           (ignore-errors
             (not
              (org-before-first-heading-p)))
           "Set Tags"
         (propertize "Set Tags" 'face 'transient-inapt-suffix))))
    ("c" org-change-tag-in-region :description
     (lambda () 			 (if
           (ignore-errors
             (org-region-active-p))
           "Change tag in region"
         (propertize "Change tag in region" 'face 'transient-inapt-suffix))))
    ""
    ("s" org-set-property :description
     (lambda () 			 (if
           (ignore-errors
             (not
              (org-before-first-heading-p)))
           "Set property"
         (propertize "Set property" 'face 'transient-inapt-suffix))))
    ("C" "Column view of properties" org-columns)
    ("i" "Insert Column View DBlock" org-columns-insert-dblock)]])


(transient-define-prefix org-extra-menu-todolists ()
  "Transient menu for TODO Lists commands."
  :refresh-suffixes t
  ["org -> TODO Lists"
   [("t" "TODO/DONE/-" org-todo)
    ("s" "Select keyword" org-extra-menu-selectkeyword)
    ("S" "Show TODO Tree" org-show-todo-tree)
    ("g" "Global TODO list" org-todo-list)
    ""
    ("e" (lambda ()
           (interactive)
           (customize-variable 'org-enforce-todo-dependencies))
     :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Enforce dependencies"
                                               org-enforce-todo-dependencies "+"
                                               " " "[" "]"))
     :transient t)
    ""
    ("d" org-toggle-ordered-property
     :description
     (lambda ()
       (let ((descr
              (org-extra--bar-make-toggle-description "Do Children sequentially"
                                                      (org-entry-get nil
                                                                     "ORDERED")
                                                      "*" " " "(" ")")))
         (if
             (ignore-errors org-enforce-todo-dependencies)
             descr
           (propertize descr 'face 'transient-inapt-suffix))))
     :transient t)
    ("D" org-toggle-ordered-property
     :description
     (lambda ()
       (let ((descr
              (org-extra--bar-make-toggle-description "Do Children parallel"
                                                      (not
                                                       (org-entry-get nil
                                                                      "ORDERED"))
                                                      "*" " " "(" ")")))
         (if
             (ignore-errors org-enforce-todo-dependencies)
             descr
           (propertize descr 'face 'transient-inapt-suffix))))
     :transient t)
    ""
    ("," "Set Priority" org-priority)
    ("p" "Priority Up" org-shiftup :transient t)
    ("P" "Priority Down" org-shiftdown :transient t)
    ""
    ("G" "Get news from all feeds" org-feed-update-all)
    ("T" "Go to the inbox of a feed..." org-feed-goto-inbox)
    ("c" "Customize feeds" (lambda ()
                             (interactive)
                             (customize-variable 'org-feed-alist)))]])


(transient-define-prefix org-extra-menu-selectkeyword ()
   "Transient menu for Select keyword commands." :refresh-suffixes t
  ["org -> TODO Lists -> Select keyword"
   [("n" org-shiftright :description
     (lambda () 			 (if
           (ignore-errors
             (org-at-heading-p))
           "Next keyword"
         (propertize "Next keyword" 'face 'transient-inapt-suffix)))
     :transient t)
    ("p" org-shiftleft :description
     (lambda () 			 (if
           (ignore-errors
             (org-at-heading-p))
           "Previous keyword"
         (propertize "Previous keyword" 'face 'transient-inapt-suffix)))
     :transient t)
    ("c" pcomplete :description
     (lambda () 			 (if
           (ignore-errors
             (assq :todo-keyword
                   (org-context)))
           "Complete Keyword"
         (propertize "Complete Keyword" 'face 'transient-inapt-suffix))))
    ("N" org-shiftcontrolright :description
     (lambda () 			 (if
           (ignore-errors
             (and
              (>
               (length org-todo-sets)
               1)
              (org-at-heading-p)))
           "Next keyword set"
         (propertize "Next keyword set" 'face 'transient-inapt-suffix)))
     :transient t)
    ("P" org-shiftcontrolright :description
     (lambda () 			 (if
           (ignore-errors
             (and
              (>
               (length org-todo-sets)
               1)
              (org-at-heading-p)))
           "Previous keyword set"
         (propertize "Previous keyword set" 'face 'transient-inapt-suffix)))
     :transient t)]])


(transient-define-prefix org-extra-menu-hyperlinks ()
  "Transient menu for Hyperlinks commands."
  :refresh-suffixes t
  ["org -> Hyperlinks"
   [("s" "Store Link (Global)" org-store-link)
    ("f" "Find existing link to here" org-occur-link-in-agenda-files)
    ("l" "Insert Link" org-insert-link)
    ("o" "Follow Link" org-open-at-point)
    ""
    ("n" "Next link" org-next-link :transient t)
    ("p" "Previous link" org-previous-link :transient t)
    ""
    ("d" org-toggle-link-display
     :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Descriptive Links"
                                              org-link-descriptive
                                              "*" " " "(" ")"))
     :transient t)
    ("L" org-toggle-link-display
     :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Literal Links"
                                              (not org-link-descriptive)
                                              "*" " " "(" ")"))
     :transient t)]])




(transient-define-prefix org-extra-menu-editing ()
  "Transient menu for Editing commands."
  :refresh-suffixes t
  ["org -> Editing"
   [("e" "Emphasis..." org-emphasize)
    ("," "Add block structure" org-insert-structure-template)
    ("'" "Edit Source Example" org-edit-special)
    ""
    ("f" "Footnote new/jump" org-footnote-action)
    ("F" "Footnote extra" org-extra-footnote)]])


(transient-define-prefix org-extra-menu-editstructure ()
  "Transient menu for Edit Structure commands."
  :refresh-suffixes t
  ["org -> Edit Structure"
   [("m" org-metaup
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-heading-p))
           "Move Subtree Up"
         (propertize "Move Subtree Up" 'face 'transient-inapt-suffix)))
     :transient t)
    ("M" org-metadown
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-heading-p))
           "Move Subtree Down"
         (propertize "Move Subtree Down" 'face 'transient-inapt-suffix)))
     :transient t)
    ""
    ("c" org-copy-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Copy Subtree"
         (propertize "Copy Subtree" 'face 'transient-inapt-suffix))))
    ("C" org-cut-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Cut Subtree"
         (propertize "Cut Subtree" 'face 'transient-inapt-suffix))))
    ("p" org-paste-special
     :description
     (lambda ()
       (if
           (ignore-errors
             (not
              (org-at-table-p)))
           "Paste Subtree"
         (propertize "Paste Subtree" 'face 'transient-inapt-suffix)))
     :transient t)
    ""
    ("s" "Clone subtree, shift time" org-clone-subtree-with-time-shift)
    ""
    ("v" "Copy visible text" org-copy-visible)
    ""
    ("P" org-metaleft :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Promote Heading"
         (propertize "Promote Heading" 'face 'transient-inapt-suffix))))
    ("S" org-shiftmetaleft :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Promote Subtree"
         (propertize "Promote Subtree" 'face 'transient-inapt-suffix))))
    ("d" org-metaright :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Demote Heading"
         (propertize "Demote Heading" 'face 'transient-inapt-suffix))))
    ("D" org-shiftmetaright :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Demote Subtree"
         (propertize "Demote Subtree" 'face 'transient-inapt-suffix))))
    ""
    ("^" "Sort Region/Children" org-sort)
    ""
    ("t" "Convert to odd levels" org-convert-to-odd-levels)
    ("T" "Convert to odd/even levels" org-convert-to-oddeven-levels)
    ("w" org-refile :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Refile Subtree"
         (propertize "Refile Subtree" 'face 'transient-inapt-suffix))))
    ("r" org-refile-copy :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Refile and copy Subtree"
         (propertize "Refile and copy Subtree" 'face 'transient-inapt-suffix))))]])


(transient-define-prefix org-extra-menu-navigateheadings ()
  "Transient menu for Navigate Headings commands."
  :refresh-suffixes t
  :transient-non-suffix #'transient--do-stay
  :transient-suffix #'transient--do-call
  ["org -> Navigate Headings"
   [("u" "Up" outline-up-heading :transient t)
    ("n" "Next" outline-next-visible-heading :transient t)
    ("p" "Previous" outline-previous-visible-heading :transient t)
    ("f" "Next Same Level" outline-forward-same-level :transient t)
    ("b" "Previous Same Level" outline-backward-same-level :transient t)
    ""
    ("j" "Jump" org-goto  :transient nil)]])


;;;###autoload (autoload 'org-extra-menu-showhide "org-extra" nil t)
(transient-define-prefix org-extra-menu-showhide ()
  "Transient menu for Show/Hide commands."
  :refresh-suffixes t
  :transient-suffix #'transient--do-call
  ["org -> Show/Hide"
   [("TAB" "Cycle Visibility" org-cycle
     :inapt-if-not (lambda ()
                     (ignore-errors
                       (or
                        (bobp)
                        (outline-on-heading-p))))
     :transient t)
    ("c" org-shifttab
     :description
     (lambda ()
       (if
           (ignore-errors
             (not
              (org-at-table-p)))
           "Cycle Global Visibility"
         (propertize "Cycle Global Visibility" 'face 'transient-inapt-suffix)))
     :transient t)
    ("/" "Sparse Tree..." org-sparse-tree :transient nil)
    ("r" "Reveal Context" org-fold-reveal)
    ("s" "Show All" org-fold-show-all)
    ""
    ("S" "Subtree to indirect buffer" org-tree-to-indirect-buffer
     :transient nil)]])


;;;###autoload (autoload 'org-extra-org-mode-menu "org-extra" nil t)
(transient-define-prefix org-extra-org-mode-menu ()
  "Define a menu for Org mode with various actions."
  [("s" "Show/Hide" org-extra-show-hide-menu)
   ("n" "New Heading" org-insert-heading)
   ("a" "Navigate Headings" org-extra-navigate-headings-menu)
   ("e" "Edit Structure" org-extra-edit-structure-menu)
   ("d" "Editing" org-extra-editing-menu)
   ("r" "Archive" org-extra-menu-archive)
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

;;;###autoload (autoload 'org-extra-c-x-menu "org-extra" nil t)
(transient-define-prefix org-extra-c-x-menu ()
  "Provide toggle, navigation, timer, and clock actions for Org mode."
  :transient-non-suffix #'transient--do-stay
  :refresh-suffixes t
  [[("TAB" "Show/Hide" org-extra-menu-showhide)
    ""
    ("," "Images" org-toggle-inline-images :transient t)
    ("." "Toggle" org-extra-toggle-menu :transient nil)
    ("N" "Navigate Headings" org-extra-menu-navigateheadings)
    ("e" "Edit Structure" org-extra-menu-editstructure)
    ("E" "Editing" org-extra-menu-editing)
    ("a" "Archive" org-extra-menu-archive)
    ""
    ("h" "Hyperlinks" org-extra-menu-hyperlinks)
    ""
    ("t" "TODO Lists" org-extra-menu-todolists)
    ("T" "TAGS and Properties" org-extra-menu-tagsandproperties)
    ("d" "Dates and Scheduling" org-extra-menu-datesandscheduling)
    ("l" "Logging work" org-extra-logging-work-menu)
    ""
    ("A" "Agenda Command..." org-agenda)
    ("<" "Set Restriction Lock" org-agenda-set-restriction-lock)
    (">" "Remove Restriction Lock" org-agenda-remove-restriction-lock)
    ("f" "File List for Agenda" org-extra-menu-filelistforagenda)
    ("v" "Special views current file" org-extra-menu-specialviewscurrentfile)
    ""
    ("x" "Export/Publish..." org-export-dispatch)
    ("L" "LaTeX" org-extra-menu-latex)
    ""
    ("D" "Documentation" org-extra-menu-documentation)
    ("C" "Customize" org-extra-menu-customize)
    ("b" "Send bug report" org-submit-bug-report)
    ""
    ("!" "Refresh/Reload" org-extra-refresh-reload-menu)]
   ["Insert"
    ("ih" "New Heading" org-insert-heading)
    ("ii" "item" org-insert-item)
    ("il" "link" org-insert-link)
    ("id" "drawer" org-insert-drawer)
    ("iL" "all-links" org-insert-all-links)
    ("is" "subheading" org-insert-subheading)
    ("ig" "link-global" org-insert-link-global)
    ("it" "todo-heading" org-insert-todo-heading)
    ("iD" "columns-dblock" org-insert-columns-dblock)
    ("iT" "todo-subheading" org-insert-todo-subheading)
    ("iS" "last-stored-link" org-insert-last-stored-link)
    ("ie" "structure-template" org-insert-structure-template)
    ("ia" "heading-after-current" org-insert-heading-after-current)
    ("C-<return>" "heading-respect-content" org-insert-heading-respect-content)
    ("ir" "todo-heading-respect-content" org-insert-todo-heading-respect-content)]
   [("g" "Get news from all feeds " org-feed-update-all)
    ("G" "Go to the inbox of a feed..." org-feed-goto-inbox)
    ("c" "Clock/Timer" org-extra-menu-clock :transient nil)
    ("C-c" org-extra-columns-toggle :description
     (lambda ()
       (org-extra--bar-make-toggle-description "Column view of properties"
                                               org-columns-current-fmt "+"
                                               "" "[" "]")))
    ("C-y" "Paste special" org-paste-special)
    ("M-w" org-copy-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Copy Rectangle"
         (propertize "Copy Rectangle" 'face 'transient-inapt-suffix))))
    ("o" "Footnote extra" org-extra-footnote)
    ("C-w" org-cut-special :description
     (lambda ()
       (if
           (ignore-errors
             (org-at-table-p))
           "Cut Rectangle"
         (propertize "Cut Rectangle" 'face 'transient-inapt-suffix))))]
   [("M-<left>" org-metaleft
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Promote Heading"
         (propertize "Promote Heading" 'face 'transient-inapt-suffix)))
     :transient t)
    ("M-S-<left>" org-shiftmetaleft
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Promote Subtree"
         (propertize "Promote Subtree" 'face 'transient-inapt-suffix)))
     :transient t)
    ("M-<right>" org-metaright
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Demote Heading"
         (propertize "Demote Heading" 'face 'transient-inapt-suffix)))
     :transient t)
    ("M-S-<right>" org-shiftmetaright
     :description
     (lambda ()
       (if
           (ignore-errors
             (org-in-subtree-not-table-p))
           "Demote Subtree"
         (propertize "Demote Subtree" 'face 'transient-inapt-suffix)))
     :transient t)]]
  (interactive)
  (require 'org-colview)
  (transient-setup #'org-extra-c-x-menu))

;;;###autoload (autoload 'org-extra-gcal "org-extra" nil t)
(transient-define-prefix org-extra-gcal ()
  "Select and invoke an EasyPG command from a list of available commands."
  :transient-suffix     #'transient--do-call
  :transient-non-suffix #'transient--do-stay
  :refresh-suffixes t
  [("D" "Delete entry at point to current calendar" org-gcal-delete-at-point
    :inapt-if-not org-extra--on-gcal-entry
    :transient nil)
   ("p" "Post entry at point to current calendar" org-gcal-post-at-point
    :inapt-if-not org-extra--on-gcal-entry
    :transient nil)]
  [("s" "Sync all events" org-gcal-sync
    :transient nil)
   ("y" "Sync buffer with Calendar (fetch and post)" org-gcal-sync-buffer
    :transient nil)
   ("f" "Fetch event data from google calendar" org-gcal-fetch
    :transient nil)
   ("e" "Fetch without posting changes" org-gcal-fetch-buffer
    :transient nil)]
  [("d" org-gcal-toggle-debug
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Debug"
                                              (bound-and-true-p org-gcal-debug)
                                              "+"
                                              "" "[" "]"))
    :transient t)
   ("u" org-gcal--sync-unlock
    :description
    (lambda ()
      (org-extra--bar-make-toggle-description "Deactivate sync lock "
                                              (bound-and-true-p
                                               org-gcal--sync-lock)
                                              "+"
                                              "" "[" "]"))
    :inapt-if-nil org-gcal--sync-lock)
   ("O" "Setup OAuth2 authentication after setting client id and secret"
    org-gcal-reload-client-id-secret
    :transient nil)
   ("C" "Clear all Calendar api sync tokens" org-gcal-sync-tokens-clear
    :transient nil)])

(defun org-extra--in-gcal-buffer ()
  "Check if current buffer is associated with a Google Calendar file."
  (when-let ((file buffer-file-name))
    (seq-find (pcase-lambda (`(,_k . ,gcal-file))
                (and gcal-file
                     (string=
                      file
                      (expand-file-name
                       gcal-file))))
              (and (boundp 'org-gcal-fetch-file-alist)
                   org-gcal-fetch-file-alist))))

(defun org-extra--on-gcal-entry ()
  "Check for Google Calendar managed property at point."
  (require 'org-gcal nil t)
  (when (bound-and-true-p org-gcal-managed-property)
    (unless (bound-and-true-p org-gcal--sync-lock)
      (org-entry-get (point) org-gcal-managed-property))))

(defun org-extra--gcal-invoke-menu ()
  "Display Google Calendar menu for valid entries."
  (require 'org-gcal nil t)
  (when (and (org-extra--in-gcal-buffer)
             (org-extra--on-gcal-entry))
    (transient-setup 'org-extra-gcal)))

;;;###autoload
(define-minor-mode org-extra-gcal-mode
  "Toggle Google Calendar menu in Org mode.

Toggle integration of Google Calendar with Org mode, adding a custom action to
`\\[org-ctrl-c-ctrl-c]' command on calendar entries."
  :lighter " org-extra-gcal"
  :global nil
  (remove-hook 'org-ctrl-c-ctrl-c-final-hook #'org-extra--gcal-invoke-menu
               'local)
  (when org-extra-gcal-mode
    (add-hook 'org-ctrl-c-ctrl-c-hook #'org-extra--gcal-invoke-menu nil
              'local)))



(provide 'org-extra)
;;; org-extra.el ends here