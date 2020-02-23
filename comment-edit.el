;;; comment-edit.el --- Code block in comment or docstring -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/06
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "2.0") (edit-indirect "0.1.5"))
;; URL: https://github.com/twlz0ne/comment-edit.el
;; Keywords: tools languages docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; [![Build Status](https://travis-ci.com/twlz0ne/comment-edit.el.svg?branch=master)](https://travis-ci.com/twlz0ne/comment-edit.el)

;; # comment-edit.el

;; Edit comment or docstring or code block inside them with your favorite mode.

;;     +----------+         Edit           +-----------+         Edit           +-----------+
;;     |          | ---------------------> |   edit    | ---------------------> |   edit    | ...
;;     |          |  point-at-comment?     |   buffer  |  point-at-comment?     |   buffer  |
;;     |  source  |  point-at-string?      |           |  point-at-string?      |           | ...
;;     |  buffer  |  point-at-codeblock?   | (markdown |  point-at-codeblock?   | (markdown | ...
;;     |          |                        |  orgmode  |                        |  orgmode  |
;;     |          | <--------------------- |   ...)    | <--------------------- |   ...)    | ...
;;     +----------+     Commit changes     +-----------+     Commit changes     +-----------+

;; ## Installation

;; Clone this repository to `~/.emacs.d/site-lisp/comment-edit`.  Add the following to your `.emacs`:

;; ```elisp
;; (require 'comment-edit)
;; (define-key prog-mode-map (kbd "C-c '") #'comment-edit)
;; (setq comment-edit-default-mode 'markdown-mode) ;; or org-mode
;; ```

;; ## Usage

;; - Move the cursor to a comment or string, or a code block inside them.
;; - <kbd>C-c '</kbd>.

;;     or press <kbd>C-u C-c '</kbd> to starting edit with manually selected major mode.

;; ## Edit comment

;; `comment-edit` use **continuity** as basis for determing whether it is a comment **block** or **line**.
;; Continuous means that there is no barrier (e.g. code or blank line) between the end of previous line and the beginning of next line, for example:

;;     /*
;;      * this is a
;;      * comment block
;;      */

;;     //
;;     // this is also a
;;     // comment block
;;     //

;;     //
;;     // this is another
;;     // comment block
;;     //

;;     code 1 /* all this are comment lines */
;;     code 2 /* all this are comment lines */
;;     code 3 // all this are comment lines
;;     code 4 // all this are comment lines

;; By setting `comment-edit-default-mode` to choose the mode (e.g. `markdown-mode` or `org-mode`) for edit buffer.
;; In edit buffer, the comment delimiter will be removed, for example:

;;     source buffer     ->    edit buffer   ->    edit buffer

;;     /*
;;      * # Example            # Example
;;      *
;;      * ``` C                ``` C
;;      * foo("bar");          foo("bar");         foo("bar");
;;      * ```                  ```
;;      */

;;     // * Example            * Example
;;     //
;;     // #+BEGIN_SRC C        #+BEGIN_SRC C
;;     // foo("bar");          foo("bar");         foo("bar");
;;     // #+END_SRC            #+END_SRC

;; ## Edit string

;; `comment-edit` provides convenience for editing escaped strings, if there are nested string or code block, just continue press <kbd>C-c '</kbd> to enter a new edit buffer:

;;     source buffer     ->    edit buffer   ->    edit buffer

;;     "a\"b\\\"c\\\"\""       a"b\"c\""           b"c"

;; ## Edit code block

;; `comment-edit` also support for editing code block directly in comment or string:

;;     source buffer     ->    eidt buffer

;;     ",--- elisp
;;      | (foo \"bar\")        (foo "bar")
;;      `---"

;;     /*
;;      * ``` C
;;      * foo("bar");          foo("bar");
;;      * ```
;;      */

;; If the language identifier of code block is omitted, the edit buffer uses the same mode as the source buffer.

;; ## Screencasts

;; <p float="left" align="center">
;;   <img src="images/comment-edit1.gif" />
;;   <img src="images/comment-edit2.gif" />
;; </p>

;; <i>P.S.</i> The language identifier of code block can be omitted in these cases.

;;; Change Log:

;;  0.1.0  2019/04/06  Initial version.

;;; Code:

(require 'dash)
(require 'edit-indirect)
(require 'calc-misc)
(require 'subr-x)

(defcustom comment-edit-default-mode 'fundamental-mode
  "Default mode for editing comment or docstring file."
  :group 'comment-edit
  :type 'symbolp)

(defcustom comment-edit-code-block-default-mode 'normal-mode
  "Default mode to use for editing code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'comment-edit
  :type '(choice function (const :tag "None" nil)))

(defcustom comment-edit-code-lang-modes
  '(("C"                . c-mode)
    ("C++"              . c++-mode)
    ("asymptote"        . asy-mode)
    ("bash"             . sh-mode)
    ("calc"             . fundamental-mode)
    ("cpp"              . c++-mode)
    ("ditaa"            . artist-mode)
    ("dot"              . fundamental-mode)
    ("elisp"            . (emacs-lisp-mode lisp-interaction-mode))
    ("ocaml"            . tuareg-mode)
    ("screen"           . shell-script-mode)
    ("shell"            . sh-mode)
    ("sqlite"           . sql-mode))
  "Alist mapping languages to their major mode.
Taken from `markdown-code-lang-modes'."
  :group 'comment-edit
  :type 'alist)

(defcustom comment-edit-not-support-docstring-modes
  '(c-mode c++-mode java-mode js-mode rust-mode)
  "A list of modes not support docstring."
  :group 'comment-edit
  :type 'list)

(defcustom comment-edit-comment-delimiter-alist
  '((("//+" "\\*+")    . (c-mode
                          c++-mode
                          csharp-mode
                          css-mode
                          go-mode
                          java-mode
                          js-mode
                          objc-mode
                          php-mode
                          swift-mode))
    (("//+!" "//+" "\\*+") . rust-mode)
    (("--")            . (applescript-mode haskell-mode lua-mode))
    (("//+")           . (pascal-mode fsharp-mode))
    ((";+")            . (emacs-lisp-mode
                          lisp-interaction-mode
                          common-lisp
                          racket-mode
                          scheme-mode))
    (("#+")            . (python-mode ruby-mode)))
  "Alist of comment delimiter regexp."
  :group 'comment-edit
  :type 'alist)

(defcustom comment-edit-comment-encloser-alist
  '((("/\\*+" "\\*/")       . (c-mode
                               c++-mode
                               csharp-mode
                               css-mode
                               go-mode
                               java-mode
                               js-mode
                               objc-mode
                               php-mode
                               rust-mode
                               swift-mode))
    (("{-" "-}")       . haskell-mode)
    (("{" "}")         . pascal-mode)
    (("(\\*" "\\*)")       . (applescript-mode fsharp-mode ocaml-mode))
    (("#|" "#|")       . (common-lisp racket-mode scheme-mode))
    (("<!--" "-->")    . (html-mode xml-mode))
    (("--\\[[" "--\\]\\]")   . lua-mode)
    (("=begin" "=end") . ruby-mode))
  "Alist mapping comment encloser to their major mode."
  :group 'comment-edit
  :type 'alist)

(defcustom comment-edit-block-regexp-plist
  '(;; Example:
    ;; ```
    ;; (message "hello, world")
    ;; ```
    (:beginning "```\s?\\(\\w*\\)$"   :middle nil    :end "```$")
    ;; Example:
    ;; ,---
    ;; | (message "hello, world")
    ;; `---
    (:beginning ",---+\s?\\(\\w*\\)$" :middle "|\s?" :end "`---+$")
    (:beginning "Local Variables:$" :middle nil :end "End:$" :mode emacs-lisp-mode))
  "Alist of block regexp."
  :group 'comment-edit
  :type 'alist)

(defcustom comment-edit-remove-trailing-spaces-in-comment nil
  "Remove trailing spaces in comment."
  :group 'comment-edit
  :type 'boolean)

(defcustom comment-edit-continue-fill-column nil
  "Use the remaining fill with of the source buffer in edit buffer."
  :group 'comment-edit
  :type 'boolean)

(defcustom comment-edit-buffer-creation-hook nil
  "Functions called after the edit buffer is created."
  :group 'comment-edit
  :type 'hook)

(defvar comment-edit--line-delimiter nil "Comment delimiter of each editing line.")

(defvar comment-edit-debug-p nil)

(defvar comment-edit-leave-blank-line-in-comment nil
  "Leave blank lines in comment without left-padding.")

;;; Utils

(defun comment-edit-toggle-debug (&optional force-p)
  "Toggle whether or not to enable to print debug.

Force enable if FORCE-P is not nil."
  (interactive)
  (setq comment-edit-debug-p (or force-p (not comment-edit-debug-p)))
  (message "comment-edit-debug-p => %s" comment-edit-debug-p))

(defun comment-edit--log (format-string &rest args)
  "Log message to the ’*comment-log*’ buffer.

FORMAT-STRING and ARGS is the same as for `message'."
  (when comment-edit-debug-p
    (if noninteractive
        (apply 'message format-string args)
      (with-current-buffer (get-buffer-create "*comment-log*")
        (outline-mode)
        (buffer-disable-undo)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (apply 'format (cons format-string args))
                  "\n"))))))

(defun comment-edit--end-of-previous-line (&optional pos)
  "Move cursor to the end of prevous line of the given point POS.

Return nil if reached the beginning of the buffer."
  (when pos
    (goto-char pos))
  (condition-case err
      (forward-line -1)
    (error nil))
  (end-of-line)
  t)

(defun comment-edit--beginning-of-next-line (&optional pos)
  "Move cursor to the beginning of next line of the given point POS.

Return nil if reached the end of the buffer."
  (when pos
    (goto-char pos))
  (condition-case err
      (forward-line 1)
    (error nil))
  (beginning-of-line)
  t)

(defun comment-edit--rassoc (item list)
  "Return non-nil if ITEM ‘eq’ to or contained in the cdr of an element of LIST."
  (cl-rassoc item
             list
             :test
             (lambda (item it)
               (if (listp it)
                   (memq item it)
                 (eq it item)))))

(defun comment-edit--get-real-mode (&optional mode)
  "Return real name of ‘major-mode’ or given MODE."
  (let ((mode (or mode major-mode)))
    (or (and (symbolp (symbol-function mode))
             (symbol-function mode))
        mode)))

;;; Docstring funcitons

(defcustom comment-edit-string-quotes-alist
  '((python-mode     . ("\"\"\"" "'''" "\"" "'"))
    (js-mode         . ("\"" "'"))
    (comment-edit-double-quote-string-mode . t)
    (comment-edit-single-quote-string-mode . ("'"))
    (t               . ("\"")))
  "Alist of string quotes."
  :group 'comment-edit
  :type 'alist)

(defun comment-edit--point-at-string ()
  "Determine if point at string or not."
  (nth 3 (syntax-ppss)))

(defun comment-edit--string-beginning ()
  "Return beginning of string at point."
  (save-excursion
    (while (comment-edit--point-at-string) (backward-char))
    (point)))

(defun comment-edit--string-end ()
  "Return end of string at point."
  (save-excursion
    (while (comment-edit--point-at-string) (forward-char))
    (point)))

(defun comment-edit--string-quotes (pos &optional backwardp mode)
  "Return quote characters around string at POS.

If BACKWARDP is not nil, search backward.
If MODE is nil, use ‘major-mode’."
  (let* ((mode (or mode major-mode))
         (looking-fn (if backwardp 'looking-back 'looking-at))
         (quotes (comment-edit-get-mode-quotes mode)))
    (save-excursion
      (when pos
        (goto-char pos))
      (catch 'break
        (while quotes
          (let ((s (pop quotes)))
            (when (funcall looking-fn s)
              (throw 'break s))))))))

(defun comment-edit--string-region (&optional pos)
  "Return region of string at point POS."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (if (comment-edit--point-at-string)
          (let* ((fbeg (comment-edit--string-beginning))
                 (fend (comment-edit--string-end))
                 (quotes-len (length (comment-edit--string-quotes fbeg))))
            (list (+ (or fbeg (point-min)) quotes-len)
                  (- (or fend (point-max)) quotes-len)))
        (user-error "Not inside a string")))))

;;; Comment functions

(defun comment-edit--comment-delimiter-regexp (&optional mode)
  "Return comment delimiter regex of MODE."
  (let* ((mode (or mode major-mode))
         (def (or (comment-edit--rassoc mode comment-edit-comment-delimiter-alist)
                  (comment-edit--rassoc (get mode 'derived-mode-parent)
                                        comment-edit-comment-delimiter-alist)
                  (comment-edit--rassoc (comment-edit--get-real-mode mode)
                                        comment-edit-comment-delimiter-alist))))
    (when def
      (if (symbolp (car def))
          (comment-edit--comment-delimiter-regexp (car def))
        (concat "^\s*\\(?:"
                (mapconcat 'identity (car def) "\\|")
                "\\)\s?")))))

(defun comment-edit--point-at-comment-exclusive-one-line ()
  "Determine if comment exclusive one line and return the comment face."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (goto-char (point-min))
      (and (re-search-forward "[^\s\t]" nil t 1)
           (and (ignore-errors (forward-char 1) t)
                (comment-edit--point-at-comment))))))

(defun comment-edit--point-at-comment (&optional point)
  "Return the face if POINT at comment."
  (let ((face (get-text-property (or point (point)) 'face)))
    (or (memq face '(font-lock-comment-face font-lock-comment-delimiter-face))
        (when (apply #'derived-mode-p comment-edit-not-support-docstring-modes)
          (memq face '(font-lock-doc-face))))))

(defun comment-edit--comment-beginning (&optional pos)
  "Look at the first line of comment from point POS.

Example:

           .- beginning
          /
        +|----------------------.
        ||      ;; comment      |
        |       ;; comment      |
        |       ;; comment      |
        '-----------------------+"
  (let ((point-at-comment-p nil)
        (point-at-newline-p nil)))
  (while (and (setq point-at-comment-p
                    (or (comment-edit--point-at-comment)
                        (comment-edit--point-at-comment-exclusive-one-line)))
              (setq point-at-newline-p (ignore-errors (backward-char 1) t))))
  (when (and (not point-at-comment-p)
             point-at-newline-p)
    (forward-char 1))
  (point))

(defun comment-edit--comment-end (&optional pos)
  "Look at the last line of comment from point POS.

Example:

        +-----------------------.
        |       ;; comment      |
        |       ;; comment      |
        |       ;; comment     ||
        '----------------------|+
                              /
                        end -`"
  (let ((point-at-comment-p nil)
        (point-at-newline-p nil))
    (while (and (setq point-at-comment-p
                      (or (comment-edit--point-at-comment)
                          (comment-edit--point-at-comment-exclusive-one-line)))
                (setq point-at-newline-p
                      (condition-case err
                          (progn
                            (forward-char 1)
                            t)
                        (error
                         (setq point-at-comment-p
                               (or (comment-edit--point-at-comment)
                                   (comment-edit--point-at-comment-exclusive-one-line)))
                         nil)))))
    (when (and point-at-newline-p
               (not point-at-comment-p)
               (not (> (point) (point-at-bol))))
      (backward-char 1))
    (point)))

(defun comment-edit--comment-region (&optional pos)
  "Return the region of continuous comments at POS.

Style 1:

          .- beginning
         /
        +------------------------.
        |       ;; comment       |
        |       ;; comment ______.
        |       ;; comment|
        '-----------------+
                         /
                   end -`

Style 2:

          .- beginning
         /      /*
        +------------------------.
        |        * comment       |
        |        * comment ______.
        |        * comment|
        '-----------------+
                 */      /
                   end -`"
  (let ((pos (or pos (point))))
    (comment-edit--log "==> [comment-edit--comment-region] pos: %s" pos)
    ;; (comment-edit--log "==> [comment-edit--comment-region] buffer string: %S"
    ;;          (buffer-substring-no-properties (point-min) (point-max)))
    (if (or (comment-edit--point-at-comment pos)
            (comment-edit--point-at-comment-exclusive-one-line))
        (let* ((fbeg (save-excursion (comment-edit--comment-beginning pos)))
               (fend (save-excursion (comment-edit--comment-end       pos)))
               (enclosed-p (comment-edit--enclosed-comment-p fbeg fend))
               (multi-line-p (not (= (line-number-at-pos fbeg)
                                     (line-number-at-pos fend)))))
          (list (save-excursion
                  (goto-char fbeg)
                  (if enclosed-p
                      ;; Skip `/*' for C/C++
                      (re-search-forward
                       (comment-edit--comment-begin-encloser multi-line-p)
                       nil t))
                  (point))
                (save-excursion
                  (goto-char fend)
                  (when enclosed-p
                    ;; Skip `*/' for C/C++
                    (re-search-backward
                     (comment-edit--comment-end-encloser multi-line-p)
                     nil t)
                    (when (and (not multi-line-p) (= (char-before) ?\s))
                      (backward-char 1)))
                  (point))))
      (user-error "Not inside a comment"))))

(defun comment-edit--comment-begin-encloser (&optional multi-line-p mode)
  "Return a regexp to match the beginning of enclosed comment.

MULTI-LINE-P means whether the comment is multi-line.
If MODE is nil, use ‘major-mode’."
  (concat (caar (comment-edit--get-comment-encloser (or mode major-mode)))
          "[\t\s]*"
          (when multi-line-p
            "\n")))

(defun comment-edit--comment-end-encloser (&optional multi-line-p mode)
  "Return a regexp to match the end of enclosed comment.

MULTI-LINE-P means whether the comment is multi-line.
If MODE is nil, use ‘major-mode’."
  (concat (when multi-line-p
            "\n")
          "[\t\s]*"
          (cl-cadar (comment-edit--get-comment-encloser (or mode major-mode)))))

(defun comment-edit--get-comment-encloser (&optional mode)
  "Return a list in the form of ‘((begin-encloser end-enclose) mode1 mode2...)’ for MODE."
  (comment-edit--rassoc (or mode major-mode)
                        comment-edit-comment-encloser-alist))

(defun comment-edit--enclosed-comment-p (&optional comment-beginning comment-end)
  "Determine if the comment from COMMENT-BEGINNING to COMMENT-END is enclosed."
  (-when-let (encloser (car (comment-edit--get-comment-encloser major-mode)))
    (and (save-excursion
           (if comment-beginning
               (goto-char comment-beginning)
             (comment-edit--comment-beginning))
           (re-search-forward (car encloser) nil t 1))
         (save-excursion
           (if comment-end
               (goto-char comment-end)
             (comment-edit--comment-end))
           (ignore-errors (forward-char 1))
           (re-search-backward (cadr encloser) nil t 1))
         t)))

;;; Code block functions

(defun comment-edit--code-block-beginning (&optional comment-delimiter)
  "Return code block info containing ‘:beginning’.

Search process will skip characters COMMENT-DELIMITER at beginning of each line."
  (let ((regexp-group
         (concat
          comment-delimiter
          "\\(?:"
          (mapconcat
           (lambda (it)
             (plist-get it :beginning))
           comment-edit-block-regexp-plist
           "\\|")
          "\\)")))
    (catch 'break
      (save-excursion
        (comment-edit--log "==> [code-block-beginning] comment-delimiter: %S" comment-delimiter)
        (comment-edit--log "==> [code-block-beginning] regexp-group: %S" regexp-group)
        (when (re-search-backward regexp-group nil t)
          (save-match-data
            (comment-edit--log "==> [code-block-beginning] matched1: %s" (match-string-no-properties 1))
            (comment-edit--log "==> [code-block-beginning] language: %s" (comment-edit-get-mode-lang major-mode)))
          (comment-edit--beginning-of-next-line)
          (throw 'break
                 (let ((block-regexp
                        (car
                         (-non-nil
                          (--map (when (string-match-p
                                        (plist-get it :beginning)
                                        (match-string-no-properties 0))
                                   it)
                                 comment-edit-block-regexp-plist)))))
                   (list
                    :beginning (point-at-bol)
                    :lang-mode
                    (or (plist-get block-regexp :mode)
                        (comment-edit-get-lang-mode (or (match-string-no-properties 1) ""))
                        major-mode)
                    :regexps block-regexp))))))))

(defun comment-edit--code-block-end (code-info &optional comment-delimiter)
  "Return CODE-INFO with ‘:end’ added.

Search process will skip characters COMMENT-DELIMITER at beginning of each line."
  (save-excursion
    (let ((regexp (concat comment-delimiter
                          (plist-get
                           (plist-get code-info :regexps)
                           :end))))
      (when (re-search-forward regexp nil t)
        (comment-edit--end-of-previous-line)
        (plist-put code-info
                   :end (point-at-eol))))))

(defun comment-edit-get-lang-mode (lang)
  "Return major mode that should be used for LANG.

LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   'fboundp
   (-flatten
    (list (cdr (assoc lang comment-edit-code-lang-modes))
          (cdr (assoc (downcase lang) comment-edit-code-lang-modes))
          (intern (concat lang "-mode"))
          (intern (concat (downcase lang) "-mode"))))))

(defun comment-edit-get-mode-lang (mode)
  "Return language of mode MODE."
  (car (cl-rassoc mode
                  comment-edit-code-lang-modes
                  :test
                  (lambda (mode it)
                    (if (listp it)
                        (memq mode it)
                      (eq it mode))))))

(defun comment-edit-get-mode-quotes (mode)
  "Return a list of quote string for MODE."
  (let ((aval (assoc-default mode comment-edit-string-quotes-alist)))
    (cond ((symbolp aval) (assoc-default (or aval t) comment-edit-string-quotes-alist))
          (t aval))))

(defun comment-edit--block-info ()
  "Return block info at point.

Block info example:

    '(:beginning 10
      :lang-mode emacs-lisp-mode
      :regexps (:beginning \"``` ?\\(\\w*\\)$\" :middle nil :end \"```$\")
      :end 12
      :in-str-p nil)

:regexps        not nil means point at a code block.
:in-str-p       not nil means point at a string block otherwish a comment block."
  (let* ((strp (comment-edit--point-at-string))
         (comment-or-string-region
          (if strp
              (let ((region (comment-edit--string-region)))
                (setq strp (comment-edit--string-quotes
                            (comment-edit--string-beginning)))
                region)
            (when (or (derived-mode-p 'prog-mode)
                      (memq major-mode '(markdown-mode org-mode)))
              (comment-edit--comment-region)))))
    (save-restriction
      (when comment-or-string-region
        (apply 'narrow-to-region comment-or-string-region))
      (let* ((delimiter (unless strp (comment-edit--comment-delimiter-regexp)))
             (code-info (comment-edit--code-block-end
                         (comment-edit--code-block-beginning delimiter)
                         delimiter)))
        (if (and (plist-get code-info :beginning) (plist-get code-info :end))
            (plist-put code-info :in-str-p strp)
          (if comment-or-string-region
              (plist-put
               (plist-put
                (plist-put code-info :beginning (point-min))
                :end (point-max))
               :in-str-p strp)
            (user-error "Not inside a code block")))))))

;;; comment-edit-mode

(defvar comment-edit-entry-key (kbd "C-c '")
  "The default entry key in editing buffer.
It will override by the key that `comment-edit' binding in source buffer.")

(defvar comment-edit-exit-key (kbd "C-c C-c")
  "The default exit key in editing buffer.")

(defvar comment-edit-abort-key (kbd "C-c C-k")
  "The default abort key in editing buffer.")

(defvar comment-edit--mode-history nil "Mode select history in each buffer.")
(make-variable-buffer-local 'comment-edit--mode-history)

(defun comment-edit--entry-key ()
  "Return `comment-edit-entry-key' or the key that `comment-edit' binding in source buffer."
  (or (car (where-is-internal
            'comment-edit-cl
            overriding-local-map))
      comment-edit-entry-key))

(defun comment-edit--buffer-creation-setup ()
  "Function called after the edit-indirect buffer is created."
  (-if-let (entry-cmd
            (pcase (or (derived-mode-p 'prog-mode) major-mode)
              ((or `comment-edit-single-quote-string-mode
                   `comment-edit-double-quote-string-mode
                   `fundamental-mode
                   `prog-mode)
               #'comment-edit)
              (`markdown-mode
               #'markdown-edit-code-block)
              (`org-mode
               #'org-edit-special)))
      (progn
        (comment-edit--log "==> [-buffer-creation-setup] major-mode: %s, entry-cmd: %s" major-mode entry-cmd)
        (use-local-map edit-indirect-mode-map)
        (local-set-key (comment-edit--entry-key) entry-cmd)
        (local-set-key comment-edit-exit-key #'edit-indirect-commit)
        (local-set-key comment-edit-abort-key #'edit-indirect-abort)
        (when comment-edit-continue-fill-column
          (setq-local fill-column (- fill-column (length comment-edit--line-delimiter))))
        (setq-local header-line-format
                    (substitute-command-keys
                     (concat "*EDIT* "
                                    (mapconcat
                                     'identity
                                     (-non-nil
                                      (list "\\[edit-indirect-commit]: Exit"
                                            "\\[edit-indirect-abort]: Abort"
                                            (format "\\[%s]: Enter" entry-cmd)))
                                     ", "))))
        (run-hooks 'comment-edit-buffer-creation-hook))
    (warn "Unknown major-mode: %s" major-mode)))

(defun comment-edit--remove-comment-delimiter (regexp)
  "Remove comment delimiter of each line by REGEXP when entering comment edit buffer."
  (let ((line-delimiter)
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (catch 'break
        (while (and (< (point-min) (point)) (re-search-backward regexp nil t))
          (unless line-delimiter
            (setq line-delimiter (match-string 0)))
          (replace-match "")
          (when (eq (point) (point-min))
            (throw 'break nil))
          (backward-char))))
    line-delimiter))

(defun comment-edit--restore-comment-delimiter (beg end)
  "Restore comment delimiter of each line between BEG to END when returning from comemntdown-mode."
  (comment-edit--log "==> [comment-edit--restore-comment-delimiter] line delimiter: %s"
                     comment-edit--line-delimiter)
  (when (and (string-prefix-p "*edit-indirect " (buffer-name))
             comment-edit--line-delimiter
             (not (string-empty-p comment-edit--line-delimiter)))
    (let ((delimiter (if (string-suffix-p " " comment-edit--line-delimiter)
                         comment-edit--line-delimiter
                       (concat comment-edit--line-delimiter " "))))
      (save-excursion
        (goto-char beg)
        (catch 'end-of-buffer
          (while (re-search-forward "^.*$" nil t)
            (let* ((str (string-trim-right (match-string 0)))
                   (line (concat delimiter str))
                   (leave-blank-p (and comment-edit-leave-blank-line-in-comment
                                       (string-empty-p str))))
              (replace-match "")
              (insert (if leave-blank-p ""
                        (if comment-edit-remove-trailing-spaces-in-comment
                            (string-trim-right line)
                          line)))
              (when leave-blank-p
                (unless (zerop (forward-line 1))
                  (throw 'end-of-buffer nil))))))))))

(defun comment-edit--remove-nested-escape ()
  "Remove escape of nested string."
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (1- (math-pow 2 num))))
        (when (and (< 0 match-len)
                   (looking-back (eval `(rx (not (any "\\")) (= ,match-len "\\"))) 1))
          (let ((del-len (- match-len (1- (math-pow 2 (1- num))))))
            (backward-delete-char del-len)
            (throw 'break nil)))))))

(defun comment-edit--remove-nested-escape-sq ()
  "Remove escape of nested single-quote string."
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (- (math-pow 2 num) 2)))
        (when (and (< 0 match-len)
                   (looking-back (eval `(rx (not (any "\\")) (= ,match-len "\\"))) 1))
          (let ((del-len (- match-len (1- (math-pow 2 (1- num))))))
            (backward-delete-char del-len)
            (throw 'break nil)))))))

(defun comment-edit--remove-escape (quotes-char)
  "Remove escape when editing docstring.

QUOTES-CHAR should be \" or '."
  (goto-char (point-min))
  (cond ((string= quotes-char "\"")
         (while (re-search-forward "\\\"" nil t)
           (replace-match "")
           (comment-edit--remove-nested-escape)
           (insert "\"")))
        ((string= quotes-char "'")
         (while (search-forward "\\'" nil t)
           (replace-match "")
           (comment-edit--remove-nested-escape-sq)
           (insert "'")))))

(defun comment-edit--restore-nested-escape ()
  "Restore escape of nested string."
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (1- (math-pow 2 (1- num)))))
        (when (and (< 0 match-len)
                   (looking-back (eval `(rx (not (any "\\")) (= ,match-len "\\"))) 1))
          (let ((pad-len (- (1- (math-pow 2 num)) match-len 1)))
            (dotimes (_ pad-len)
              (princ "\\")
              (insert "\\"))
            (throw 'break nil)))))))

(defun comment-edit--restore-escape (quotes-char)
  "Restore escape when finished edting docstring.

QUOTES-CHAR should be \" or '."
  (goto-char (point-min))
  (cond ((string= quotes-char "\"")
         (while (search-forward "\"" nil t)
           (replace-match "")
           (comment-edit--restore-nested-escape)
           (insert "\\\"")))
        ((string= quotes-char "'")
         (while (search-forward "'" nil t)
           (replace-match "")
           (comment-edit--restore-nested-escape)
           (insert "\\'")))))

(defalias 'comment-edit--derived-mode-p
    (if (fboundp 'provided-mode-derived-p) 'provided-mode-derived-p
      ;; From Emacs 26
      (lambda (mode &rest modes)
        "Non-nil if MODE is derived from one of MODES or their aliases.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
        (while
            (and
             (not (memq mode modes))
             (let* ((parent (get mode 'derived-mode-parent))
                    (parentfn (symbol-function parent)))
               (setq mode (if (and parentfn (symbolp parentfn)) parentfn parent)))))
        mode)))

(defun comment-edit--select-mode ()
  "Select major mode."
  (completing-read
   "Select mode: "
   (lambda (string pred action)
     (append comment-edit--mode-history
             (let ((pred
                     (lambda (sym)
                       (and (funcall pred sym)
                            (or (comment-edit--derived-mode-p sym 'prog-mode)
                                (memq sym
                                      '(text-mode
                                        markdown-mode
                                        org-mode
                                        comment-edit-mode
                                        comment-edit-single-quote-string-mode
                                        comment-edit-double-quote-string-mode)))))))
               (complete-with-action action obarray string pred))))
   #'commandp t nil 'comment-edit--mode-history (or (car comment-edit--mode-history)
                                                    (format "%s" major-mode))))

(defvar comment-edit-mode-map (make-sparse-keymap) "Keymap used in comment edit buffer.")

(define-minor-mode comment-edit-mode
  "Minor mode for enable edit code block in comment.\\{comment-edit-mode-map}"
  :init-value nil
  :group 'comment-edit
  :global nil
  :keymap 'comment-edit-mode-map)

(defvar comment-edit-double-quote-string-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (comment-edit--entry-key) #'comment-edit)
    map)
  "Keymap for `comment-edit-double-quote-string-mode'.")

(define-generic-mode 'comment-edit-double-quote-string-mode
  nil nil nil nil
  '((lambda ()
      (modify-syntax-entry ?' "\"")
      (use-local-map comment-edit-double-quote-string-mode-map)))
  "Major mode for editing double-quoted string.")

(defvar comment-edit-single-quote-string-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (comment-edit--entry-key) #'comment-edit)
    map)
  "Keymap for `comment-edit-single-quote-string-mode'.")

(define-generic-mode 'comment-edit-single-quote-string-mode
  nil nil nil nil
  '((lambda ()
      (modify-syntax-entry ?' "\"")
      (use-local-map comment-edit-single-quote-string-mode-map)))
  "Major mode for editing single-quoted string.")

;;;###autoload
(defun comment-edit (&optional block)
  "Edit comment or docstring or code BLOCK in them.

Normally, the major mode of the edit buffer will be selected automatically,
but users can also manually select it by pressing `C-u \\[comment-edit]'."
  (interactive "P")
  (let* ((block (or block (comment-edit--block-info)))
         (beg (plist-get block :beginning))
         (end (plist-get block :end))
         (lang-mode (plist-get block :lang-mode))
         (strp (plist-get block :in-str-p))
         (commentp (not strp))
         (codep (and (plist-get block :regexps) t))
         (delimiter-regexp (concat (if strp "^\s*"
                                     (comment-edit--comment-delimiter-regexp))
                                   (plist-get (plist-get block :regexps) :middle)))
         (edit-indirect-after-creation-hook #'comment-edit--buffer-creation-setup))
    (comment-edit--log "==> block-info: %S" block)
    ;; (comment-edit--log "==> block: %S" (buffer-substring-no-properties beg end))
    (if block
        (let* ((mode (if current-prefix-arg
                         (intern (comment-edit--select-mode))
                       (if codep
                           (or lang-mode
                               comment-edit-code-block-default-mode)
                         (cond ((and (stringp strp) (string= "'" strp)) 'comment-edit-single-quote-string-mode)
                               ((and (stringp strp) (string= "\"" strp)) 'comment-edit-double-quote-string-mode)
                               (t comment-edit-default-mode))))))
          (setq-local edit-indirect-guess-mode-function
                      `(lambda (_parent-buffer _beg _end)
                         (let ((line-delimiter (and (or ,codep ,commentp) (comment-edit--remove-comment-delimiter ,delimiter-regexp))))
                           (comment-edit--log "==> block(edit buffer): %S" (buffer-substring-no-properties (point-min) (point-max)))
                           (when ,strp
                             (comment-edit--log "==> quotes(edit buffer): %S" ,strp)
                             (comment-edit--remove-escape ,strp))
                           (comment-edit--log "==> mode(edit buffer): %S" ',mode)
                           (funcall ',mode)
                           (set (make-local-variable 'comment-edit-leave-blank-line-in-comment)
                                ,comment-edit-leave-blank-line-in-comment)
                           (set (make-local-variable 'comment-edit--line-delimiter) line-delimiter)
                           (set (make-local-variable 'edit-indirect-before-commit-hook)
                                (append '((lambda ()
                                            (comment-edit--restore-comment-delimiter (point-min) (point-max))
                                            (when ,strp
                                              (comment-edit--restore-escape ,strp))))
                                        edit-indirect-before-commit-hook)))))
          (edit-indirect-region beg end 'display-buffer))
      (user-error "Not inside a edit block"))))

(provide 'comment-edit)

;;; comment-edit.el ends here
