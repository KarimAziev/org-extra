;;; org-extra.el --- Configure extra -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-extra
;; Keywords: outlines
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (org "9.6.3") (transient "0.4.1"))
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
  (let ((langs))
    (dolist (it features)
      (let ((name (symbol-name it)))
        (when (and
               (string-prefix-p "ob-"
                                name)
               (not (member name '("ob-core" "ob-eval" "ob-table" "ob-tangle")))
               (not (string-suffix-p "autoloads" name)))
          (push (substring-no-properties name 3) langs))))
    langs))

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

(defcustom org-extra-eldoc-flags-functions	'(org-extra-eldoc-documentation-function)
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
           (append (org-get-outline-path) (list cur))
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
  (let ((element (save-match-data (org-element-at-point))))
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
(defvar org-extra-eldoc-special-props '(("ALLTAGS" .
                                         "All tags, including inherited ones.")
                                        ("BLOCKED" . "‘t’ if task is currently blocked by children or siblings.")
                                        ("CATEGORY" . "The category of an entry.")
                                        ("CLOCKSUM" .
                                         "The sum of CLOCK intervals in the subtree.  ‘org-clock-sum’\n                 must be run first to compute the values in the current buffer.")
                                        ("CLOCKSUM_T" .
                                         "The sum of CLOCK intervals in the subtree for today.\n                 ‘org-clock-sum-today’ must be run first to compute the\n                 values in the current buffer.")
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
  '(("'" .
     "Toggle smart quotes (‘org-export-with-smart-quotes’).  Depending on\n     the language used, when activated, Org treats pairs of double\n     quotes as primary quotes, pairs of single quotes as secondary\n     quotes, and single quote marks as apostrophes.")
    ("*" . "Toggle emphasized text (‘org-export-with-emphasize’).")
    ("-" .
     "Toggle conversion of special strings\n     (‘org-export-with-special-strings’).")
    (":" . "Toggle fixed-width sections (‘org-export-with-fixed-width’).")
    ("<" .
     "Toggle inclusion of time/date active/inactive stamps\n     (‘org-export-with-timestamps’).")
    ("\\n" .
     "Toggles whether to preserve line breaks\n     (‘org-export-preserve-breaks’).")
    ("^" .
     "Toggle TeX-like syntax for sub- and superscripts.  If you write\n     ‘^:{}’, ‘a_{b}’ is interpreted, but the simple ‘a_b’ is left as it\n     is (‘org-export-with-sub-superscripts’).")
    ("arch" .
     "Configure how archived trees are exported.  When set to ‘headline’,\n     the export process skips the contents and processes only the\n     headlines (‘org-export-with-archived-trees’).")
    ("author" .
     "Toggle inclusion of author name into exported file\n     (‘org-export-with-author’).")
    ("broken-links" .
     "Toggles if Org should continue exporting upon finding a broken\n     internal link.  When set to ‘mark’, Org clearly marks the problem\n     link in the output (‘org-export-with-broken-links’).")
    ("c" . "Toggle inclusion of ‘CLOCK’ keywords (‘org-export-with-clocks’).")
    ("creator" .
     "Toggle inclusion of creator information in the exported file\n     (‘org-export-with-creator’).")
    ("d" .
     "Toggles inclusion of drawers, or list of drawers to include, or\n     list of drawers to exclude (‘org-export-with-drawers’).")
    ("date" .
     "Toggle inclusion of a date into exported file\n     (‘org-export-with-date’).")
    ("e" . "Toggle inclusion of entities (‘org-export-with-entities’).")
    ("email" .
     "Toggle inclusion of the author’s e-mail into exported file\n     (‘org-export-with-email’).")
    ("f" . "Toggle the inclusion of footnotes (‘org-export-with-footnotes’).")
    ("H" .
     "Set the number of headline levels for export\n     (‘org-export-headline-levels’).  Below that level, headlines are\n     treated differently.  In most back-ends, they become list items.")
    ("inline" .
     "Toggle inclusion of inlinetasks (‘org-export-with-inlinetasks’).")
    ("num" .
     "Toggle section-numbers (‘org-export-with-section-numbers’).  When\n     set to number N, Org numbers only those headlines at level N or\n     above.  Set ‘UNNUMBERED’ property to non-‘nil’ to disable numbering\n     of heading and subheadings entirely.  Moreover, when the value is\n     ‘notoc’ the headline, and all its children, do not appear in the\n     table of contents either (see *note Table of Contents::).")
    ("p" .
     "Toggle export of planning information (‘org-export-with-planning’).\n     “Planning information” comes from lines located right after the\n     headline and contain any combination of these cookies: ‘SCHEDULED’,\n     ‘DEADLINE’, or ‘CLOSED’.")
    ("pri" .
     "Toggle inclusion of priority cookies (‘org-export-with-priority’).")
    ("prop" .
     "Toggle inclusion of property drawers, or list the properties to\n     include (‘org-export-with-properties’).")
    ("stat" .
     "Toggle inclusion of statistics cookies\n     (‘org-export-with-statistics-cookies’).")
    ("tags" .
     "Toggle inclusion of tags, may also be ‘not-in-toc’\n     (‘org-export-with-tags’).")
    ("tasks" .
     "Toggle inclusion of tasks (TODO items); or ‘nil’ to remove all\n     tasks; or ‘todo’ to remove done tasks; or list the keywords to keep\n     (‘org-export-with-tasks’).")
    ("tex" .
     "‘nil’ does not export; ‘t’ exports; ‘verbatim’ keeps everything in\n     verbatim (‘org-export-with-latex’).")
    ("timestamp" .
     "Toggle inclusion of the creation time in the exported file\n     (‘org-export-time-stamp-file’).")
    ("title" . "Toggle inclusion of title (‘org-export-with-title’).")
    ("toc" .
     "Toggle inclusion of the table of contents, or set the level limit\n     (‘org-export-with-toc’).")
    ("todo" .
     "Toggle inclusion of TODO keywords into exported text\n     (‘org-export-with-todo-keywords’).")
    ("|" .
     "Toggle inclusion of tables (‘org-export-with-tables’).\n\n   When exporting subtrees, special node properties can override the\nabove keywords.  These properties have an ‘EXPORT_’ prefix.  For\nexample, ‘DATE’ becomes, ‘EXPORT_DATE’ when used for a specific subtree.\nExcept for ‘SETUPFILE’, all other keywords listed above have an")))

(defvar org-extra-eldoc-short-export-setting
  '(("AUTHOR" . "The document author (‘user-full-name’).")
    ("CREATOR" .
     "Entity responsible for output generation\n     (‘org-export-creator-string’).")
    ("DATE" . "A date or a time-stamp(2).")
    ("EMAIL" . "The email address (‘user-mail-address’).")
    ("LANGUAGE" .
     "Language to use for translating certain strings\n     (‘org-export-default-language’).  With ‘#+LANGUAGE: fr’, for\n     example, Org translates ‘Table of contents’ to the French ‘Table\n     des matières’(3).")
    ("SELECT_TAGS" .
     "The default value is ‘(\"export\")’.  When a tree is tagged with\n     ‘export’ (‘org-export-select-tags’), Org selects that tree and its\n     subtrees for export.  Org excludes trees with ‘noexport’ tags, see\n     below.  When selectively exporting files with ‘export’ tags set,\n     Org does not export any text that appears before the first\n     headline.")
    ("EXCLUDE_TAGS" .
     "The default value is ‘(\"noexport\")’.  When a tree is tagged with\n     ‘noexport’ (‘org-export-exclude-tags’), Org excludes that tree and\n     its subtrees from export.  Entries tagged with ‘noexport’ are\n     unconditionally excluded from the export, even if they have an\n     ‘export’ tag.  Even if a subtree is not exported, Org executes any\n     code blocks contained there.")
    ("TITLE" .
     "Org displays this title.  For long titles, use multiple ‘#+TITLE’ lines.")
    ("EXPORT_FILE_NAME" .
     "The name of the output file to be generated.  Otherwise, Org\n     generates the file name based on the buffer name and the extension\n     based on the back-end format.\n\n   The ‘OPTIONS’ keyword is a compact form.  To configure multiple\noptions, use several ‘OPTIONS’ lines.  ‘OPTIONS’ recognizes the\nfollowing arguments.")
    ("ARCHIVE" . "Sets the archive location of the agenda file")
    ("CONSTANTS" .
     "Set file-local values for constants that table formulas can use")
    ("FILETAGS" . "#+FILETAGS: :tag1:tag2:tag3:")
    ("LINK"
     .
     "#+LINK: linkword replace Each line specifies one abbreviation for one link")
    ("PROPERTY" . "#+PROPERTY: Property_Name Value")
    ("ALLTAGS" .
     "All tags, including inherited ones.")
    ("BLOCKED" .
     "‘t’ if task is currently blocked by children or siblings.")
    ("CATEGORY" .
     "The category of an entry.")
    ("CLOCKSUM" .
     "The sum of CLOCK intervals in the subtree.  ‘org-clock-sum’\n                 must be run first to compute the values in the current buffer.")
    ("CLOCKSUM_T" .
     "The sum of CLOCK intervals in the subtree for today.\n                 ‘org-clock-sum-today’ must be run first to compute the\n                 values in the current buffer.")
    ("CLOSED" .
     "When was this entry closed?")
    ("DEADLINE" . "The deadline timestamp.")
    ("FILE" .
     "The filename the entry is located in.")
    ("ITEM" . "The headline of the entry.")
    ("PRIORITY" .
     "The priority of the entry, a string with a single letter.")
    ("SCHEDULED" .
     "The scheduling timestamp.")
    ("TAGS" .
     "The tags defined directly in the headline. (‘org-tag-alist’)")
    ("TIMESTAMP" .
     "The first keyword-less timestamp in the entry.")
    ("TIMESTAMP_IA" .
     "The first inactive timestamp in the entry.")
    ("TODO" .
     "The TODO keyword of the entry.")
    ("DESCRIPTION" .
     "This is the document’s description, which the HTML exporter inserts\n     it as a HTML meta tag in the HTML file.  For long descriptions, use\n     multiple ‘DESCRIPTION’ lines.  The exporter takes care of wrapping\n     the lines properly.\n\n     The exporter includes a number of other meta tags, which can be\n     customized by modifying ‘org-html-meta-tags’.")
    ("HTML_DOCTYPE" .
     "Specify the document type, for example: HTML5 (‘org-html-doctype’).")
    ("HTML_CONTAINER" .
     "Specify the HTML container, such as ‘div’, for wrapping sections\n     and elements (‘org-html-container-element’).")
    ("HTML_LINK_HOME" . "The URL for home link (‘org-html-link-home’).")
    ("HTML_LINK_UP" .
     "The URL for the up link of exported HTML pages\n     (‘org-html-link-up’).")
    ("HTML_MATHJAX" .
     "Options for MathJax (‘org-html-mathjax-options’).  MathJax is used\n     to typeset LaTeX math in HTML documents.  See *note Math formatting\n     in HTML export::, for an example.")
    ("HTML_HEAD" .
     "Arbitrary lines for appending to the HTML document’s head\n     (‘org-html-head’).")
    ("HTML_HEAD_EXTRA" .
     "More arbitrary lines for appending to the HTML document’s head\n     (‘org-html-head-extra’).")
    ("KEYWORDS" .
     "Keywords to describe the document’s content.  HTML exporter inserts\n     these keywords as HTML meta tags.  For long keywords, use multiple\n     ‘KEYWORDS’ lines.")
    ("LATEX_HEADER" .
     "Arbitrary lines for appending to the preamble; HTML exporter\n     appends when transcoding LaTeX fragments to images (see *note Math\n     formatting in HTML export::).")
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
  "Substitute STR with variable values in BUFF."
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
(defun org-extra-example-blocks-to-org-src (language)
  "Convert example blocks in buffer to begin/end_SUFFIX blocks with LANGUAGE.
If LANGUAGE is omitted, read it with completions."
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

;;;###autoload
(defun org-extra-reload ()
  "Reload Org uncompiled."
  (interactive)
  (org-reload t))

;;;###autoload (autoload 'org-extra-refresh-reload-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-refresh-reload-menu ()
  "Transient menu for Refresh/Reload commands."
  [("r" "Refresh setup current buffer" org-mode-restart)
   ("e" "Reload Org (after update) (C-c C-x !)" org-reload)
   ("l" "Reload Org uncompiled" org-extra-reload)])

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

;;;###autoload
(defun org-extra-clock-in ()
  "Offer a list of \ recently clocked tasks to clock into."
  (interactive)
  (org-clock-in '(4)))

;;;###autoload
(defun org-extra-clock-in-mark ()
  "Clock into the current task and mark it as the default task.
A special task that will always be offered in the
clocking selection, associated with the letter `d'."
  (interactive)
  (org-clock-in '(16)))

;;;###autoload (autoload 'org-extra-logging-work-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-logging-work-menu ()
  "Transient menu for Logging work commands."
  [("c" "Clock in (C-c C-x TAB)" org-clock-in)
   ("s" "Switch task" org-extra-clock-in)
   ("l" "Clock out (C-c C-x C-o)" org-clock-out)
   ("o" "Clock cancel (C-c C-x C-q)" org-clock-cancel)
   ("m" "Mark as default task" org-clock-mark-default-task)
   ("k" "Clock in, mark as default" org-extra-clock-in-mark)
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

;;;###autoload
(defun org-extra-customize-org-enforce-todo-dependencies ()
  "Customize `org-enforce-todo-dependencies'."
  (interactive)
  (customize-variable 'org-enforce-todo-dependencies))

;;;###autoload
(defun org-extra-customize-feed ()
  "Customize `org-feed-alist'."
  (interactive)
  (customize-variable 'org-feed-alist))

;;;###autoload (autoload 'org-extra-todo-lists-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-todo-lists-menu ()
  "Transient menu for TODO Lists commands."
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

;;;###autoload
(defun org-extra-footnote ()
  "Offer special org footnote actions."
  (interactive)
  (org-footnote-action t))

;;;###autoload (autoload 'org-extra-editing-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-editing-menu ()
  "Transient menu for Editing commands."
  [("e" "Emphasis... (C-c C-x C-f)" org-emphasize)
   ("a" "Add block structure (C-c C-,)" org-insert-structure-template)
   ("d" "Edit Source Example (C-c ')" org-edit-special)
   ("f" "Footnote new/jump (C-c C-x f)" org-footnote-action)
   ("o" "Footnote extra" org-extra-footnote)])

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

;;;###autoload
(defun org-extra-babel-menu ()
  "Remove remove all result blocks in the buffer."
  (interactive)
  (org-babel-remove-result-one-or-many t))

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
    ("D" "remove all" org-extra-babel-menu)
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
  "Insert a new row below the current line into the table."
  (interactive)
  (org-table-insert-row t))

;;;###autoload (autoload 'org-extra-menu-org-table-transient "org-extra.el" nil t)
(transient-define-prefix org-extra-menu-org-table-transient ()
  "Command dispatcher for org table."
  :transient-suffix #'transient--do-call
  :transient-non-suffix #'transient--do-stay
  [["Org table"
    ("i" "field info" org-table-field-info)
    ("RET" "Insert row" org-extra-table-insert-row-below)
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
  (if
      (when (fboundp 'org-at-table-p)
        (org-at-table-p))
      (transient-setup 'org-extra-menu-org-table-transient)
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
  "Read documentation for Org Timers in the info system."
  (interactive)
  (org-info "Timers"))

;;;###autoload
(defun org-extra-info-clock-commands ()
  "Read documentation for Org Clocking commands in the info system."
  (interactive)
  (org-info "Clocking commands"))

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


;;;###autoload
(defun org-extra-agenda-archives-files ()
  "Toggle inclusion of files in trees marked with :ARCHIVE:."
  (interactive)
  (require 'org-agenda)
  (when (fboundp 'org-agenda-archives-mode)
    (org-agenda-archives-mode 'files)))

;;;###autoload (autoload 'org-extra-agenda-transient "org-extra.el" nil t)
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

;;;###autoload (autoload 'org-extra-toggle-menu "org-extra.el" nil t)
(transient-define-prefix org-extra-toggle-menu ()
  "Command dispatcher with org toggle commands."
  :transient-suffix #'transient--do-call
  [["Toggle visibility"
    ("i" "Images" org-toggle-inline-images)
    ("b" "Block" org-hide-block-toggle)
    ("d" "Drawer" org-hide-drawer-toggle)
    ("C" "Custom properties" org-toggle-custom-properties-visibility)
    ("l" "Links" org-toggle-link-display)
    ("E" "Entities" org-toggle-pretty-entities)]
   ["Toggle buttons"
    ("c" "Comment" org-toggle-comment)
    ("x" "Checkbox" org-toggle-checkbox)
    ("r" "Radio" org-toggle-radio-button)
    ("a" "Archive tag" org-toggle-archive-tag)]]
  ["Toggle headings"
   ("H" "Headings => normal lines => list"
    org-toggle-item)
   ("h" "Headings => normal text" org-toggle-heading)]
  [("w" "Fixed-width markup" org-toggle-fixed-width)
   ("g" "Group tags" org-toggle-tags-groups)
   ("t" "Timestamp" org-toggle-timestamp-type)
   ("e" "Custom time stamp formats" org-toggle-time-stamp-overlays)
   ("u" "Debugging flags for ‘org-gcal’" org-gcal-toggle-debug)])


(provide 'org-extra)
;;; org-extra.el ends here