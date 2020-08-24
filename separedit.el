;;; separedit.el --- Edit comment/string/docstring/code block in separate buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/06
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4") (dash "2.0") (dash-functional "1.2") (edit-indirect "0.1.5"))
;; URL: https://github.com/twlz0ne/separedit.el
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; [![Build Status](https://travis-ci.com/twlz0ne/separedit.el.svg?branch=master)](https://travis-ci.com/twlz0ne/separedit.el)
;; [![MELPA](https://melpa.org/packages/separedit-badge.svg)](https://melpa.org/#/separedit)

;; # separedit.el

;; Edit comment/string/docstring/code block in separate buffer with your favorite mode.

;;     +----------+         Edit           +-----------+         Edit           +-----------+
;;     |          | ---------------------> |   edit    | ---------------------> |   edit    | ...
;;     |          |  point-at-comment?     |   buffer  |  point-at-comment?     |   buffer  |
;;     |  source  |  point-at-string?      |           |  point-at-string?      |           | ...
;;     |  buffer  |  point-at-codeblock?   | (markdown |  point-at-codeblock?   | (markdown | ...
;;     |          |  point-at-...?         |  orgmode  |  point-at-...?         |  orgmode  |
;;     |          | <--------------------- |   ...)    | <--------------------- |   ...)    | ...
;;     +----------+     Commit changes     +-----------+     Commit changes     +-----------+

;; ## Installation

;; Clone this repository, or install from MELPA. Add the following to your `.emacs`:

;; ``` elisp
;; (require 'separedit)

;; ;; Key binding for modes you want edit
;; ;; or simply bind ‘global-map’ for all.
;; (define-key prog-mode-map        (kbd "C-c '") #'separedit)
;; (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
;; (define-key help-mode-map        (kbd "C-c '") #'separedit)
;; (define-key helpful-mode-map     (kbd "C-c '") #'separedit)

;; ;; Default major-mode for edit buffer
;; ;; can also be other mode e.g. ‘org-mode’.
;; (setq separedit-default-mode 'markdown-mode)

;; ;; Feature options
;; ;; (setq separedit-preserve-string-indentation t)
;; ;; (setq separedit-continue-fill-column t)
;; ;; (setq separedit-write-file-when-execute-save t)
;; ;; (setq separedit-remove-trailing-spaces-in-comment t)
;; ```

;; ## Usage

;; - Move the cursor to a comment/string/code block or any supported place.
;; - Press <kbd>C-c '</kbd>.

;;     or press <kbd>C-u C-c '</kbd> to starting edit with manually selected major mode.

;; Can also press <kbd>C-c '</kbd> on an active region.

;; Following are default keys in edit buffer:

;; | Key                | Function                                           | Summary                                                             |
;; |:-------------------|:---------------------------------------------------|:--------------------------------------------------------------------|
;; | <kbd>C-c C-c</kbd> | `separedit-commit`                                 | Commit changes and close edit buffer                                |
;; | <kbd>C-x C-s</kbd> | `separedit-save`                                   | Commit changes (even write source file) without closing edit buffer |
;; | <kbd>C-c C-k</kbd> | `separedit-abort`                                  | Discard changes and close edit buffer                               |
;; | <kbd>C-c '</kbd>   | `separedit` or follow the settings of markdown/org | Open a new edit buffer                                              |

;; ### Edit comment

;; `separedit` use **continuity** as basis for determining whether it is a comment **block** or **line**.
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

;; By setting `separedit-default-mode` to choose the mode (e.g. `markdown-mode` or `org-mode`) for edit buffer.
;; In edit buffer, the comment delimiter will be removed, for example (█ represents the cursor):

;;     source buffer     ->    edit buffer   ->    edit buffer

;;     /*
;;      * # Example█           # Example
;;      *
;;      * ``` C                ``` C
;;      * foo("bar");          foo("bar");█        foo("bar");
;;      * ```                  ```
;;      */

;;     // * Example█           * Example
;;     //
;;     // #+BEGIN_SRC C        #+BEGIN_SRC C
;;     // foo("bar");          foo("bar");█        foo("bar");
;;     // #+END_SRC            #+END_SRC

;; ### Edit string

;; `separedit` provides convenience for editing escaped strings, if there are nested string or code block, just continue press <kbd>C-c '</kbd> to enter a new edit buffer:

;;     source buffer     ->    edit buffer   ->    edit buffer

;;     "a█\"b\\\"c\\\"\""       a"b█\"c\""           b"c"

;; ### Edit code block

;; `separedit` also support for editing code block directly in comment or string:

;;     source buffer     ->    edit buffer

;;     ",--- elisp
;;      | (foo \"bar\")█       (foo "bar")
;;      `---"

;;     /*
;;      * ``` C
;;      * foo("bar");█         foo("bar");
;;      * ```
;;      */

;; If the language identifier of code block is omitted, the edit buffer uses the same mode as the source buffer.

;; ### Edit value form of variable in help/helpful buffer

;; Describe a variable, move cursor to the local/global value form, press <kbd>C-c '</kbd> to edit it.

;; ### Edit minibuffer

;; Don't get stuck in minibuffer, press <kbd>C-c '</kbd> to open a edit buffer.

;; ## Customization

;; ### Change key bindings in edit buffer

;; If you don't like the default key bindings in edit buffer, you can change it:

;; - `separedit-save-key`
;; - `separedit-entry-key`
;; - `separedit-abort-key`
;; - `separedit-commit-key`

;; ### Add support for a new major mode

;; 1. Add the start/end delimiter of block style comment to `separedit-comment-encloser-alist`.
;; 1. Add the delimiter of each comment line to `separedit-comment-delimiter-alist`.
;; 1. Add the string (including docstring) quotes to `separedit-string-quotes-alist`.
;; 1. Add definition to `separedit-string-indent-offset-alist` if there is base indent offset in docstring.
;; 1. Add a mode name to `separedit-not-support-docstring-modes` if not support docstring.

;; ### Add support for a new code block

;; 1. Add a set of regexps matching the new code block to `separedit-block-regexp-plists`.
;; 1. Add a language name to `separedit-code-lang-modes` if can't get mode by simply adding suffix `-mode`.

;; ### Preserving indentation of block in string

;; If `separedit-preserve-string-indentation` is non-nil, the indentation of string block will be preseved in edit buffer, e.g:

;; ```
;; source buffer                         edit buffer
;; +--------------------+                +--------------------+
;; | def func():        |                | Usage:             |
;; |     '''            |                |     func()         |
;; |     Usage:         |       ->       |                    |
;; |         func()     |                |                    |
;; |     '''            |                |                    |
;; |     pass           |                |                    |
;; +====================+                +====================+
;; ```

;; No only for the docsting, normal string are also supported:

;; ```
;; source buffer                         edit buffer
;; +--------------------+                +--------------------+
;; | emacs \            |                | (progn             |
;; |    --batch \       |                |   ...)             |
;; |    --eval "(progn  |       ->       |                    |
;; |              ...)" |                |                    |
;; |                    |                |                    |
;; +====================+                +====================+
;; ```

;; ### Continue fill-column width in edit buffer

;; If `separedit-continue-fill-column` is non-nil, use the remaining fill-width in edit buffer:

;; ```
;; source buffer                   edit buffer

;;     //
;;     // this is a                this is a
;;     // comment block            comment block
;;     //

;; |<---->|<------------>|         |<------------->|
;;    |         |                         |
;;    |         '-- available width for --'
;;    |                edit buffer
;; used width
;; ```

;; You may also like to enable `auto-fill-mode` in edit buffer:

;; ```elisp
;; (add-hook 'separedit-buffer-creation-hook #'auto-fill-mode)
;; ```

;; ## Some extended usage

;; ### Combine multipe adjacent blocks as a single edit block

;; ``` elisp
;; (defun separedit//region-of-el-commentary ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (re-search-forward "^;;; Commentary:\n+")
;;       (let ((begin (point)))
;;         (when (re-search-forward  "\n;;; .*$" nil t)
;;           (goto-char (match-beginning 0))
;;           (list begin (point)))))))

;; (defun separedit/edit-el-commentary ()
;;   "Edit whole commentary section as a single block."
;;   (interactive)
;;   (let ((separedit-leave-blank-line-in-comment t))
;;     (separedit-dwim
;;      (apply #'separedit-mark-region
;;             `(,@(separedit/region-of-el-commentary)
;;               markdown-mode)))))
;; ```

;; ### Break long lines in comment

;; ``` elisp
;; (defun separedit/re-fill ()
;;   (interactive)
;;   (let ((separedit-continue-fill-column t))
;;     (with-current-buffer (separedit-dwim)
;;       (fill-region (point-min) (point-max))
;;       (execute-kbd-macro (kbd "C-c C-k")))))
;; ```

;; ### Eval multiple-line sexp in comment

;; ``` elisp
;; (defun separedit/eval-last-sexp-in-comment ()
;;   (interactive)
;;   (let ((separedit-default-mode 'emacs-lisp-mode))
;;     (with-current-buffer (separedit)
;;       (prog1 (call-interactively #'eval-last-sexp)
;;         (execute-kbd-macro (kbd "C-c C-k"))))))

;; (define-key emacs-lisp-mode-map (kbd "C-x C-e")
;;   (lambda ()
;;     (interactive)
;;     (call-interactively
;;      (if (separedit--point-at-comment)
;;          #'separedit/eval-last-sexp-in-comment
;;        #'eval-last-sexp))))
;; ```

;;; Change Log:

;;  0.2.0  2020/02/25  Renamed to separedit
;;  0.1.0  2019/04/06  Initial version.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'edit-indirect)
(require 'calc-misc)
(require 'subr-x)

(declare-function org-edit-special "org")
(declare-function markdown-edit-code-block "markdown-mode")
(declare-function gfm-edit-code-block "gfm-mode")

(defcustom separedit-default-mode 'fundamental-mode
  "Default mode for editing comment or docstring file."
  :group 'separedit
  :type 'symbolp)

(defcustom separedit-code-block-default-mode 'normal-mode
  "Default mode to use for editing code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'separedit
  :type '(choice function (const :tag "None" nil)))

(defcustom separedit-code-lang-modes
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
  :group 'separedit
  :type 'alist)

(defcustom separedit-not-support-docstring-modes
  '(c-mode c++-mode java-mode js-mode rust-mode rustic-mode typescript-mode)
  "A list of modes not support docstring."
  :group 'separedit
  :type 'list)

(defcustom separedit-comment-delimiter-alist
  '((("//+" "\\*+")    . (c-mode
                          c++-mode
                          csharp-mode
                          css-mode
                          go-mode
                          java-mode
                          js-mode
                          objc-mode
                          php-mode
                          swift-mode
                          typescript-mode))
    (("//+!" "//+" "\\*+") . (rust-mode
                              rustic-mode))
    (("--")            . (applescript-mode haskell-mode lua-mode))
    (("//+")           . (pascal-mode fsharp-mode))
    ((";+")            . (emacs-lisp-mode
                          lisp-interaction-mode
                          common-lisp
                          racket-mode
                          scheme-mode))
    (("#+")            . (nix-mode python-mode ruby-mode)))
  "Alist of comment delimiter regexp."
  :group 'separedit
  :type 'alist)

(defcustom separedit-comment-encloser-alist
  '((("/\\*+" "\\*+/") . (c-mode
                          c++-mode
                          csharp-mode
                          css-mode
                          go-mode
                          java-mode
                          js-mode
                          nix-mode
                          objc-mode
                          php-mode
                          rust-mode
                          rustic-mode
                          swift-mode
                          typescript-mode))
    (("{-" "-}")       . haskell-mode)
    (("{" "}")         . pascal-mode)
    (("(\\*" "\\*)")   . (applescript-mode fsharp-mode ocaml-mode))
    (("#|" "#|")       . (common-lisp racket-mode scheme-mode))
    (("<!--" "-->")    . (html-mode xml-mode))
    (("--\\[[" "--\\]\\]") . lua-mode)
    (("=begin" "=end") . ruby-mode))
  "Alist mapping comment encloser to their major mode."
  :group 'separedit
  :type 'alist)

(defcustom separedit-block-regexp-plists
  '((:header "```\s?\\([^\s\n\r]*\\)$"
     :body   ""
     :footer "```$")

    (:header "#\\+BEGIN_SRC\s\\([^\s\n\r]*\\).*$"
     :body   ""
     :footer "#\\+END_SRC$")

    (:header ",---+\s?\\([^\s\n\r]*\\)$"
     :body   "|\s?"
     :footer "`---+$")

    (:header "┌───+\s?\\([^\s\n\r]*\\)$"
     :body   "│\s?"
     :footer "└───+$")

    (:header "Local Variables:$"
     :body   ""
     :footer "End:$"
     :mode   emacs-lisp-mode))
  "Lists of regexp to match code block.

Each element of it is in the form of:

    (:header REGEX ;; to match the header    line of block
     :body   REGEX ;; to match the each body line of block
     :fotter REGEX ;; to match the footer    line of block
     :mode   MODE) ;; major mode for edit buffer (optional)"
  :group 'separedit
  :type 'alist)

(defcustom separedit-remove-trailing-spaces-in-comment nil
  "Remove trailing spaces in comment."
  :group 'separedit
  :type 'boolean)

(defcustom separedit-continue-fill-column nil
  "Use the remaining fill width of the source buffer in edit buffer."
  :group 'separedit
  :type 'boolean)

(defcustom separedit-preserve-string-indentation nil
  "If non-nil preserve leading whitespaces in string."
  :group 'separedit
  :type 'boolean)

(defcustom separedit-write-file-when-execute-save nil
  "If non-nil write file when executing ‘separedit-save’ in edit buffer."
  :group 'separedit
  :type 'boolean)

(defcustom separedit-string-indent-offset-alist
  '((nix-mode . 2))
  "AList of indentation offset of string block start at a new line.

Each item is of the form (MODE . OFFSET).

Example of a string block with indentation offset:

   str = ''
     two spaces indentation offset
   '';"
  :group 'separedit
  :type 'alist)

(defcustom separedit-buffer-creation-hook nil
  "Functions called after the edit buffer is created."
  :group 'separedit
  :type 'hook)

(defvar separedit--line-delimiter nil "Comment delimiter of each editing line.")

(defvar separedit--indent-length nil "Indent length of each editing line.")

(defvar separedit--indent-line1 nil "Whether to indent the 1st editing line of comment.")

(defvar separedit--code-block-p nil "Wheter or not editing a code block.")

(defvar separedit-debug-p nil)

(defvar separedit-leave-blank-line-in-comment nil
  "Leave blank lines in comment without left-padding.")

;;; Utils

(defun separedit-toggle-debug (&optional force-p)
  "Toggle whether or not to enable to print debug.

Force enable if FORCE-P is not nil."
  (interactive)
  (setq separedit-debug-p (or force-p (not separedit-debug-p)))
  (message "separedit-debug-p => %s" separedit-debug-p))

(defun separedit--log (format-string &rest args)
  "Log message to the ’*comment-log*’ buffer.

FORMAT-STRING and ARGS is the same as for `message'."
  (when separedit-debug-p
    (if noninteractive
        (apply #'message format-string args)
      (with-current-buffer (get-buffer-create "*separedit-log*")
        (outline-mode)
        (buffer-disable-undo)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (apply #'format (cons format-string args))
                  "\n"))))))

(defun separedit--end-of-previous-line (&optional pos)
  "Move cursor to the end of previous line of the given point POS.

Return nil if reached the beginning of the buffer."
  (when pos
    (goto-char pos))
  (condition-case _err
      (forward-line -1)
    (error nil))
  (end-of-line)
  t)

(defun separedit--beginning-of-next-line (&optional pos)
  "Move cursor to the beginning of next line of the given point POS.

Return nil if reached the end of the buffer."
  (when pos
    (goto-char pos))
  (condition-case _err
      (forward-line 1)
    (error nil))
  (beginning-of-line)
  t)

(defun separedit--rassoc (item list)
  "Return non-nil if ITEM ‘eq’ to or contained in the cdr of an element of LIST."
  (cl-rassoc item
             list
             :test
             (lambda (item it)
               (if (listp it)
                   (memq item it)
                 (eq it item)))))

(defun separedit--get-real-mode (&optional mode)
  "Return real name of ‘major-mode’ or given MODE."
  (let ((mode (or mode major-mode)))
    (or (and (symbolp (symbol-function mode))
             (symbol-function mode))
        mode)))

;;; String / Docstring functions

(defcustom separedit-string-quotes-alist
  '((python-mode     . ("\"\"\"" "'''" "\"" "'"))
    (js-mode         . ("\"" "'" "`"))
    (nix-mode        . ("''" "\""))
    (typescript-mode . ("\"" "'" "`"))
    (separedit-double-quote-string-mode . t)
    (separedit-single-quote-string-mode . ("'"))
    (t               . ("\"")))
  "Alist of string quotes."
  :group 'separedit
  :type 'alist)

(defun separedit--point-at-string ()
  "Determine if point at string or not."
  (nth 3 (syntax-ppss)))

(defun separedit--string-beginning ()
  "Return beginning of string at point."
  (save-excursion
    (while (separedit--point-at-string) (backward-char))
    (point)))

(defun separedit--string-end ()
  "Return end of string at point."
  (save-excursion
    (while (separedit--point-at-string) (forward-char))
    (point)))

(defun separedit--string-quotes (pos &optional backwardp mode)
  "Return quote characters around string at POS.

If BACKWARDP is not nil, search backward.
If MODE is nil, use ‘major-mode’."
  (let* ((mode (or mode major-mode))
         (looking-fn (if backwardp 'looking-back 'looking-at))
         (quotes (separedit-get-mode-quotes mode)))
    (save-excursion
      (when pos
        (goto-char pos))
      (catch 'break
        (while quotes
          (let ((s (pop quotes)))
            (when (funcall looking-fn s)
              (throw 'break s))))))))

(defun separedit--string-region (&optional pos)
  "Return region of string at point POS."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (if (separedit--point-at-string)
          (let* ((fbeg (separedit--string-beginning))
                 (fend (separedit--string-end))
                 (quotes-len (length (separedit--string-quotes fbeg))))
            (list (+ (or fbeg (point-min)) quotes-len)
                  (- (or fend (point-max)) quotes-len)))
        (user-error "Not inside a string")))))

;;; Comment functions

(defun separedit--comment-delimiter-regexp (&optional mode)
  "Return comment delimiter regex of MODE.

If MODE is nil, it defaults to `major-mode'.
If there is no comment delimiter regex for MODE, return `comment-start-skip'."
  (let* ((mode (or mode major-mode))
         (def (or (separedit--rassoc mode separedit-comment-delimiter-alist)
                  (separedit--rassoc (get mode 'derived-mode-parent)
                                     separedit-comment-delimiter-alist)
                  (separedit--rassoc (separedit--get-real-mode mode)
                                     separedit-comment-delimiter-alist))))
    (if def
        (if (symbolp (car def))
            (separedit--comment-delimiter-regexp (car def))
          (concat "^\s*\\(?:"
                  (mapconcat #'identity (car def) "\\|")
                  "\\)\s?"))
      comment-start-skip)))

(defun separedit--point-at-comment-exclusive-one-line ()
  "Determine if comment exclusive one line and return the comment face."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (goto-char (point-min))
      (and (re-search-forward "[^\s\t]" nil t 1)
           (and (ignore-errors (forward-char 1) t)
                (separedit--point-at-comment))))))

(defun separedit--point-at-comment (&optional point)
  "Return the face if POINT at comment."
  (let ((face (get-text-property (or point (point)) 'face)))
    (or (memq face '(font-lock-comment-face font-lock-comment-delimiter-face))
        (when (apply #'derived-mode-p separedit-not-support-docstring-modes)
          (memq face '(font-lock-doc-face))))))

(defun separedit--comment-beginning (&optional pos)
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
        (point-at-newline-p nil))
    (when pos
      (goto-char pos))
    (while (and (setq point-at-comment-p
                      (or (separedit--point-at-comment)
                          (separedit--point-at-comment-exclusive-one-line)))
                (setq point-at-newline-p (ignore-errors (backward-char 1) t))))
    (when (and (not point-at-comment-p)
               point-at-newline-p)
      (forward-char 1))
    (point)))

(defun separedit--comment-end (&optional pos)
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
    (when pos
      (goto-char pos))
    (while (and (setq point-at-comment-p
                      (or (separedit--point-at-comment)
                          (separedit--point-at-comment-exclusive-one-line)))
                (setq point-at-newline-p
                      (condition-case _err
                          (progn
                            (forward-char 1)
                            t)
                        (error
                         (setq point-at-comment-p
                               (or (separedit--point-at-comment)
                                   (separedit--point-at-comment-exclusive-one-line)))
                         nil)))))
    (when (and point-at-newline-p
               (not point-at-comment-p)
               (not (> (point) (point-at-bol))))
      (backward-char 1))
    (point)))

(defun separedit--comment-region (&optional pos)
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
    (separedit--log "==> [separedit--comment-region] pos: %s" pos)
    ;; (separedit--log "==> [separedit--comment-region] buffer string: %S"
    ;;          (buffer-substring-no-properties (point-min) (point-max)))
    (if (or (separedit--point-at-comment pos)
            (separedit--point-at-comment-exclusive-one-line))
        (let* ((fbeg (save-excursion (separedit--comment-beginning pos)))
               (fend (save-excursion (separedit--comment-end       pos)))
               (enclosed-p (separedit--enclosed-comment-p fbeg fend))
               (multi-line-p (not (= (line-number-at-pos fbeg)
                                     (line-number-at-pos fend)))))
          (list (save-excursion
                  (goto-char fbeg)
                  (when enclosed-p
                    (separedit--skip-comment-begin-encloser multi-line-p))
                  (point))
                (save-excursion
                  (goto-char fend)
                  (when enclosed-p
                    (separedit--skip-comment-end-encloser multi-line-p))
                  (point))))
      (user-error "Not inside a comment"))))

(defun separedit--skip-comment-begin-encloser (&optional multi-line-p mode)
  "Search forward from point for the beginning encloser of comment.

MULTI-LINE-P means whether the comment is multi-line.
If MODE is nil, use ‘major-mode’."
  (re-search-forward
   (concat
    (caar (separedit--get-comment-encloser (or mode major-mode)))
    "[\t\s]*"
    (when multi-line-p
      "\n?"))))

(defun separedit--skip-comment-end-encloser (&optional multi-line-p mode)
  "Search backward from point for the beginning encloser of comment.

MULTI-LINE-P means whether the comment is multi-line.
If MODE is nil, use ‘major-mode’."
  (let ((encloser-end
         (cl-cadar (separedit--get-comment-encloser (or mode major-mode)))))
    (when (re-search-backward encloser-end nil t)
      ;; Search backward greedy
      (let (pos)
        (save-excursion
          (backward-char 1)
          (while (looking-at encloser-end)
            (setq pos (point))
            (backward-char 1)))
        (when pos
          (goto-char pos)))
      (while (looking-back "[\t\s]" nil)
        (backward-char 1))
      (when (and multi-line-p (= (char-before) ?\n))
        (backward-char 1)))))

(defun separedit--get-comment-encloser (&optional mode)
  "Return a list in the form of ‘((begin-encloser end-enclose) mode1 mode2...)’ for MODE."
  (separedit--rassoc (or mode major-mode)
                     separedit-comment-encloser-alist))

(defun separedit--enclosed-comment-p (&optional comment-beginning comment-close)
  "Determine if the comment from COMMENT-BEGINNING to COMMENT-CLOSE is enclosed."
  (-when-let (encloser (car (separedit--get-comment-encloser major-mode)))
    (save-restriction
      (narrow-to-region (or comment-beginning (point-min))
                        (or comment-close     (point-max)))
      (and (save-excursion
             (if comment-beginning
                 (goto-char comment-beginning)
               (separedit--comment-beginning))
             (re-search-forward (car encloser) nil t 1))
           (save-excursion
             (if comment-close
                 (goto-char comment-close)
               (separedit--comment-end))
             (ignore-errors (forward-char 1))
             (re-search-backward (cadr encloser) nil t 1))
           t))))

;;; Code block functions

(defun separedit--code-block-beginning (&optional comment-delimiter)
  "Return code block info containing ‘:beginning’.

Search process will skip characters COMMENT-DELIMITER at beginning of each line."
  (let* ((regexp-group
          (concat
           comment-delimiter
           "\\(\s*\\)" ;; group 1
           "\\("       ;; group 2,3
           (mapconcat
            (lambda (it)
              (plist-get it :header))
            separedit-block-regexp-plists
            "\\|")
           "\\)"))
         code-block-indent)
    (catch 'break
      (save-excursion
        (separedit--log "==> [code-block-beginning] comment-delimiter: %S" comment-delimiter)
        (separedit--log "==> [code-block-beginning] regexp-group: %S" regexp-group)
        (when (re-search-backward regexp-group nil t)
          (setq code-block-indent (current-column))
          (save-match-data
            (separedit--log "==> [code-block-beginning] cbindent: %s" code-block-indent)
            (separedit--log "==> [code-block-beginning] matched1: %s" (match-string-no-properties 2))
            (separedit--log "==> [code-block-beginning] language: %s" (separedit-get-mode-lang major-mode)))
          (separedit--beginning-of-next-line)
          (throw 'break
                 (let* ((leading-spaces (match-string-no-properties 1))
                        (matched-group
                         (-non-nil
                          (cl-loop for n from 2 to (1- (length (match-data)))
                                   collect (if (match-string n) n))))
                        (block-regexp
                         (car
                          (-non-nil
                           (--map (when (string-match-p
                                         (plist-get it :header)
                                         (match-string-no-properties (car matched-group)))
                                    it)
                                  separedit-block-regexp-plists)))))
                   (list
                    :code-block-indent code-block-indent
                    :beginning (point-at-bol)
                    :lang-mode
                    (or (plist-get block-regexp :mode)
                        (separedit-get-lang-mode
                         (or (cadr (mapcar #'match-string-no-properties matched-group))
                             ""))
                        major-mode)
                    :comment-delimiter (concat comment-delimiter leading-spaces)
                    :regexps block-regexp))))))))

(defun separedit--code-block-end (code-info)
  "Return CODE-INFO with ‘:end’ added."
  (when (and code-info (plist-get code-info :beginning))
    (save-excursion
      (goto-char (plist-get code-info :beginning))
      (let ((regexp (concat (plist-get code-info :comment-delimiter)
                            (plist-get
                             (plist-get code-info :regexps)
                             :footer))))
        (separedit--log "==> [code=block-end] regexp: %s" regexp)
        (when (re-search-forward regexp nil t)
          (separedit--end-of-previous-line)
          (plist-put code-info
                     :end (point-at-eol)))))))

(defun separedit-get-lang-mode (lang)
  "Return major mode that should be used for LANG.

LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   'fboundp
   (-flatten
    (list (cdr (assoc lang separedit-code-lang-modes))
          (cdr (assoc (downcase lang) separedit-code-lang-modes))
          (intern (concat lang "-mode"))
          (intern (concat (downcase lang) "-mode"))))))

(defun separedit-get-mode-lang (mode)
  "Return language of mode MODE."
  (car (cl-rassoc mode
                  separedit-code-lang-modes
                  :test
                  (lambda (mode it)
                    (if (listp it)
                        (memq mode it)
                      (eq it mode))))))

(defun separedit-get-mode-quotes (mode)
  "Return a list of quote string for MODE."
  (let ((aval (assoc-default mode separedit-string-quotes-alist)))
    (cond ((symbolp aval) (assoc-default (or aval t) separedit-string-quotes-alist))
          (t aval))))

(defun separedit--indent-of-string-block (quotes beg end)
  "Return the indentation of string block between BEN and END quoted by QUOTES."
  (save-excursion
    (goto-char beg)
    (let ((str-start (buffer-substring-no-properties (point) (point-at-eol)))
          (beg-at-newline (string= "" (string-trim
                                       (buffer-substring-no-properties
                                        (point-at-bol)
                                        (- (point) (length quotes))))))
          (end-at-newline (string= "" (string-trim
                                       (save-excursion
                                         (goto-char end)
                                         (buffer-substring-no-properties
                                          (point-at-bol)
                                          (point)))))))
      (cond ((string= str-start "\\")
             ;; For
             ;;
             ;;   "\
             ;;   String block does not need preserve indetation
             ;;   "
             nil)
            ((and (string= str-start "") beg-at-newline end-at-newline)
             ;; For
             ;;
             ;;   '''
             ;;   string block need preserve indetation
             ;;   '''
             (- (current-column) (length quotes)))
            ((and (string= str-start "") (not beg-at-newline) end-at-newline)
             ;; For
             ;;
             ;;   str = '''
             ;;   string block need preserve indetation
             ;;   '''
             (goto-char end)
             (+ (current-column)
                (or (cdr (assoc major-mode separedit-string-indent-offset-alist))
                    0)))
            ((not (string= str-start ""))
             ;; For situations like:
             ;;
             ;;   emacs --batch --eval "(progn
             ;;                           ...)"
             ;;
             (current-column))))))

(defun separedit--restore-point (line rcolumn)
  "Restore point to LINE and RCOLUMN."
  (forward-line (- line (line-number-at-pos)))
  (condition-case _err
      (goto-char (point-at-eol))
    (end-of-buffer))
  (save-restriction
    (condition-case _err
        (backward-char rcolumn)
      (beginning-of-buffer))))

(defun separedit--point-info (&optional beg end)
  "Return relative point info between BEG and END in source buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region (or beg (point-min)) (or end (point-max)))
      (list (line-number-at-pos (point))
            (- (save-excursion
                 (goto-char (point-at-eol))
                 (current-column))
               (current-column))))))

(defun separedit--block-info ()
  "Return block info at point.

Block info example:

    '(:beginning 10
      :lang-mode emacs-lisp-mode
      :comment-delimiter \"^ *\\(?:;+\\) ?  \"
      :regexps (:header \"``` ?\\(\\w*\\)$\"
                :body   \"\"
                :footer \"```$\")
      :end 12
      :string-quotes nil
      :indent-length nil
      :code-block-indent 4)

:regexps        not nil means point at a code block.
:string-quotes  not nil means point at a string block otherwise a comment block."
  (let* ((pos (point))
         (strp (separedit--point-at-string))
         (comment-or-string-region
          (if strp
              (let ((region (separedit--string-region)))
                (setq strp (separedit--string-quotes
                            (separedit--string-beginning)))
                region)
            (when (or (derived-mode-p 'prog-mode)
                      (memq major-mode '(gfm-mode markdown-mode org-mode)))
              (separedit--comment-region))))
         (indent-line1 nil)
         (string-indent
          (if (and strp separedit-preserve-string-indentation)
              (apply #'separedit--indent-of-string-block
                     strp
                     comment-or-string-region)
            (save-excursion
              (goto-char (car comment-or-string-region))
              ;; Not at "/*|"
              (unless (= (point) (point-at-eol))
                (if (= (point) (point-at-bol))
                    ;; At "| * comment"
                    (save-excursion
                      (when (re-search-forward "[\s\t]+" nil t)
                        (setq indent-line1 t)
                        (current-column)))
                  ;; At "/* |comment"
                  (current-column)))))))
    (save-restriction
      (when comment-or-string-region
        (apply #'narrow-to-region comment-or-string-region))
      (let* ((delimiter (unless strp (separedit--comment-delimiter-regexp)))
             (code-info (separedit--code-block-end
                         (separedit--code-block-beginning delimiter))))
        (if (and code-info
                 (<= (plist-get code-info :beginning) pos)
                 (<= pos (plist-get code-info :end)))
            (append code-info (list :string-quotes strp
                                    :indent-line1 indent-line1
                                    :indent-length string-indent))
          (if comment-or-string-region
              (list :beginning (point-min)
                    :end (point-max)
                    :string-quotes strp
                    :indent-line1 indent-line1
                    :indent-length string-indent)
            (user-error "Not inside a edit block")))))))

;;; Help/helpful mode variables / funcstions

(defvar separedit--inhibit-read-only nil)

(defvar separedit--help-variable-edit-info nil)

(defun separedit-described-symbol ()
  "Return the symbol described in help/helpful buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (or (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
           nil t 1)
      (intern (match-string-no-properties 0)))))

(defun separedit-described-value-bound ()
  "Return the bound of symbol value at point in help/hepful buffer."
  (save-excursion
    (catch 'break
      (while (pcase-let ((`(,depth ,start . ,_) (syntax-ppss)))
               (if (or (and (zerop depth) (not start))
                       (looking-back "^Value:\\(\n\\|\s\\)" 1))
                   (throw 'break (bounds-of-thing-at-point 'sexp))
                 (goto-char start)))))))

(defun separedit-help-variable-edit-info ()
  "Return help varible edit info (symbol value-bound type local-buffer) at point."
  (unless (eq major-mode 'help-mode)
    (user-error "Not in help buffer"))
  (let* ((symbol (separedit-described-symbol))
         (bound (separedit-described-value-bound))
         (type-buffer
          (save-excursion
            (goto-char (point-min))
            (let ((buffer?
                   (when (re-search-forward
                          "^Local in buffer \\([^;]+\\); global value is[\s]?"
                          nil t 1)
                     (match-string-no-properties 1))))
              (goto-char (car bound))
              (backward-char)
              (list (cond
                     ((or (looking-back "^Value:[\s]?" 1)
                          (looking-back "^Its value is[\s]?" 1)
                          (looking-back (format "^%s’s value is[\s]?" symbol) 1))
                      (if buffer? 'local 'global))
                     ((or (looking-back "^Original value was[\s]?" 1)
                          (looking-back "^Local in buffer \\([^;]+\\); global value is[\s]?" 1))
                      'global))
                    buffer?)))))
    (when (and symbol bound (car type-buffer))
      `(,symbol ,bound ,@type-buffer))))

(defun separedit-helpful-variable-edit-info ()
  "Return helpful variable edit info (symbol value-bound type local-buffer) at point."
  (unless (eq major-mode 'helpful-mode)
    (user-error "Not in help buffer"))
  (let* ((symbol (separedit-described-symbol))
         (bound (separedit-described-value-bound))
         (type-buffer
          (save-excursion
            (goto-char (car bound))
            (forward-line -1)
            (cond ((looking-at "^Value$") (list 'global nil))
                  ((looking-at "^Value in #<buffer \\(.*\\)>$")
                   (list 'local (match-string-no-properties 1)))
                  ((looking-at "^Original Value$") (list 'global nil))))))
    (when (and symbol bound (car type-buffer))
      `(,symbol ,bound ,@type-buffer))))

;;; separedit-mode

(defvar separedit-entry-key (kbd "C-c '")
  "The default entry key in editing buffer.
It will override by the key that `separedit' binding in source buffer.")

(defvar separedit-commit-key (kbd "C-c C-c")
  "The default commit key in editing buffer.")

(defvar separedit-save-key (kbd "C-x C-s")
  "The default save key in editing buffer.")

(defvar separedit-abort-key (kbd "C-c C-k")
  "The default abort key in editing buffer.")

(defvar separedit--mode-history nil "Mode select history in each buffer.")
(make-variable-buffer-local 'separedit--mode-history)

(defun separedit--entry-key ()
  "Return `separedit-entry-key' or the key that `separedit' binding in source buffer."
  (or (car (where-is-internal
            'separedit-cl
            overriding-local-map))
      separedit-entry-key))

(defun separedit-in-edit-buffer-p ()
  "Return t if in edit buffer."
  (string-prefix-p "*edit-indirect " (buffer-name)))

(defun separedit--apply-changes ()
  "Apply changes to source buffer."
  (let ((inhibit-read-only separedit--inhibit-read-only))
    (edit-indirect--barf-if-not-indirect)
    (if separedit--help-variable-edit-info
        (let* ((sym (nth 0 separedit--help-variable-edit-info))
               (typ (nth 2 separedit--help-variable-edit-info))
               (buf (nth 3 separedit--help-variable-edit-info))
               (val (car (read-from-string (buffer-string)))))
          (cond
           ((and (eq typ 'local) buf) (with-current-buffer buf (eval `(setq-local ,sym ',val))))
           ((and (eq typ 'global) (not buf)) (eval `(setq-default ,sym ',val)))
           (t (message "Unknown variable type: %s" typ)))
          ;; Make sure `edit-indirect--overlay' not be destroyed.
          (when (overlay-buffer edit-indirect--overlay)
            (edit-indirect--commit)))
      (edit-indirect--commit))))

(defun separedit-save ()
  "Save changes but without exiting edit buffer."
  (interactive)
  (when (separedit-in-edit-buffer-p)
    (let ((source-buffer (overlay-buffer edit-indirect--overlay))
          (edit-buffer-clone
           ;; Clone a buffer to protect the folding of text in edit buffer.
           (clone-buffer (concat (buffer-name) " <clone>")))
          (function-backup
           ;; Temprary disable the ‘edit-indirect--clean-up’ (but who calls
           ;; this function after the clone buffer is killed?)
           (symbol-function 'edit-indirect--clean-up)))
      (unwind-protect
          (with-current-buffer edit-buffer-clone
            (fset 'edit-indirect--clean-up (lambda ()))
            (separedit--apply-changes))
        (kill-buffer edit-buffer-clone)
        (fset 'edit-indirect--clean-up function-backup)
        (if (and separedit-write-file-when-execute-save
                 (buffer-file-name source-buffer))
            (with-current-buffer source-buffer
              (save-buffer))
          (message "Updated %S" source-buffer))))))

(defun separedit-commit ()
  "Commit changes."
  (interactive)
  (let ((point-info (separedit--point-info)) ;; Still at edit buffer
        (mark-beg (overlay-start edit-indirect--overlay))
        (mark-end (overlay-end edit-indirect--overlay)))
    (separedit--apply-changes)
    (edit-indirect--clean-up) ;; Returned to source buffer
    (goto-char
     (save-excursion
       (save-restriction
         (narrow-to-region mark-beg (min mark-end (point-max)))
         (apply #'separedit--restore-point point-info)
         (point))))))

(defun separedit--buffer-creation-setup ()
  "Function called after the edit-indirect buffer is created."
  (-if-let (entry-cmd
            (pcase (or (derived-mode-p 'prog-mode) major-mode)
              ((or `separedit-single-quote-string-mode
                   `separedit-double-quote-string-mode
                   `fundamental-mode
                   `prog-mode)
               #'separedit)
              (`markdown-mode
               #'markdown-edit-code-block)
              (`gfm-mode
               #'gfm-edit-code-block)
              (`org-mode
               #'org-edit-special)))
      (let ((km (copy-keymap edit-indirect-mode-map)))
        (separedit--log "==> [-buffer-creation-setup] major-mode: %s, entry-cmd: %s" major-mode entry-cmd)
        (define-key km (separedit--entry-key) entry-cmd)
        (define-key km separedit-commit-key #'separedit-commit)
        (define-key km separedit-save-key #'separedit-save)
        (define-key km separedit-abort-key #'edit-indirect-abort)
        (make-local-variable 'minor-mode-overriding-map-alist)
        (push `(edit-indirect--overlay . ,km) minor-mode-overriding-map-alist)
        (when separedit-continue-fill-column
          (setq-local fill-column (- fill-column (length separedit--line-delimiter))))
        (separedit--log "==> [-buffer-creation-setup] fill-column: %s" fill-column)
        (setq-local header-line-format
                    (substitute-command-keys
                     (concat "*EDIT* "
                             (mapconcat
                              'identity
                              (-non-nil
                               (list "\\[separedit-commit]: Finish"
                                     "\\[edit-indirect-abort]: Abort"
                                     (format "\\[%s]: Enter" entry-cmd)))
                              ", "))))
        (run-hooks 'separedit-buffer-creation-hook)
        (separedit--log "==> [-buffer-creation-setup] hooks finished"))
    (warn "Unknown major-mode: %s" major-mode)))

(defun separedit--remove-comment-delimiter (regexp &optional max-width)
  "Remove comment delimiter of each line when entering comment edit buffer.

REGEXP          regexp expression that matches the delimiter
MAX-WIDTH       maximum width that can be removed"
  (let ((line-delimiter)
        (inhibit-read-only t)
        match-len
        replace-str)
    (save-excursion
      (goto-char (point-max))
      (catch 'break
        (while (and (< (point-min) (point)) (re-search-backward regexp nil t))
          (setq match-len (length (match-string 0)))
          (setq replace-str
                (if (and max-width (> match-len max-width))
                    (setq replace-str (substring (match-string 0) max-width))
                  ""))
          (unless line-delimiter
            (setq line-delimiter
                  (if max-width
                      (if (zerop max-width)
                          ""
                        (substring (match-string 0) 0 (- (length (match-string 0))
                                                         (length replace-str))))
                    (match-string 0))))
          (replace-match replace-str)
          (when (eq (point) (point-min))
            (throw 'break nil))
          (goto-char (1- (point-at-bol))))))
    line-delimiter))

(defun separedit--restore-comment-delimiter ()
  "Restore comment delimiter of each line when returning from edit buffer."
  (separedit--log "==> [separedit--restore-comment-delimiter] line delimiter: %s"
                  separedit--line-delimiter)
  (when (and (separedit-in-edit-buffer-p)
             separedit--line-delimiter
             (not (string-empty-p separedit--line-delimiter)))
    (let ((delimiter (if (string-suffix-p " " separedit--line-delimiter)
                         separedit--line-delimiter
                       (concat separedit--line-delimiter " "))))
      (save-excursion
        (goto-char (point-min))
        (when (and (not separedit--code-block-p)
                   (not separedit--indent-line1) separedit--indent-length)
          ;; For the comment block like following:
          ;;      ,-----------,
          ;;    /*|comment    |
          ;;  ,---'       ,---'
          ;;  |  * comment|*/
          ;;  '-----------'
          (forward-line 1))
        (catch 'end-of-buffer
          (while (re-search-forward "^.*$" nil t)
            (let* ((str (string-trim-right (match-string 0)))
                   (line (concat delimiter str))
                   (leave-blank-p (and separedit-leave-blank-line-in-comment
                                       (string-empty-p str))))
              (replace-match "")
              (insert (if leave-blank-p ""
                        (if separedit-remove-trailing-spaces-in-comment
                            (string-trim-right line)
                          line)))
              (when leave-blank-p
                (unless (zerop (forward-line 1))
                  (throw 'end-of-buffer nil))))))))))

(defun separedit--remove-string-indent (indent-length)
  "Remove INDENT-LENGTH length of indentation from string.

  +---------------------+    +---------------------+
  | def function():     |    | Docstring           |
  |     '''             |    |                     |
  |     Docstring       | -> |                     |
  |     '''             |    |                     |
  |     ...             |    |                     |
  +-----\---------------+    +-\-------------------+
          indent                 indent"
  (let ((inhibit-read-only t)
        (indented-str
         (let ((buffer-str (buffer-string)))
           (catch 'break
             (with-temp-buffer
               (insert buffer-str)
               (goto-char (point-min))
               (when (looking-at-p "[^\s\t\n\r]")
                 (forward-line))
               (while (re-search-forward "[^\s\t\n\r]" nil t 1)
                 (goto-char (match-beginning 0))
                 (if (<= indent-length (current-column))
                     (delete-region (point-at-bol)
                                    (+ (point-at-bol) indent-length))
                   ;; Respect the origninal indentation
                   (throw 'break nil))
                 (forward-line))
               (buffer-string))))))
    (when indented-str
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert indented-str))
      indent-length)))

(defun separedit--restore-string-indent ()
  "Restore string indentation.

  +---------------------+    +---------------------+
  | def function():     |    | Docstring           |
  |     '''             |    |                     |
  |     Docstring       | <- |                     |
  |     '''             |    |                     |
  |     ...             |    |                     |
  +-----\---------------+    +-\-------------------+
         indent                 indent"
  (when (and (separedit-in-edit-buffer-p)
             separedit--indent-length)
    (goto-char (point-min))
    (when (and (not separedit--indent-line1)
               (looking-at-p "[^\s\t\n\r]"))
      (forward-line))
    (while (re-search-forward "[^\s\t\n\r]" nil t 1)
        (goto-char (match-beginning 0))
        (insert (make-string separedit--indent-length ?\s))
        (forward-line))))

(defun separedit--remove-nested-escape ()
  "Remove escape of nested string."
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (1- (math-pow 2 num)))
            (prefix (rx (not (any "\\")))))
        (when (and (< 0 match-len)
                   (looking-back
                    (concat (if (> (point) 2)
                                ;; If the escaped character at point
                                ;; is not the 1st character of string
                                prefix)
                            (rx-to-string `(= ,match-len "\\")))
                    1))
          (let ((del-len (- match-len (1- (math-pow 2 (1- num))))))
            (backward-delete-char del-len)
            (throw 'break nil)))))))

(defun separedit--remove-nested-escape-sq ()
  "Remove escape of nested single-quote string."
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (- (math-pow 2 num) 2)))
        (when (and (< 0 match-len)
                   (looking-back (eval `(rx (not (any "\\")) (= ,match-len "\\"))) 1))
          (let ((del-len (- match-len (1- (math-pow 2 (1- num))))))
            (backward-delete-char del-len)
            (throw 'break nil)))))))

(defun separedit--remove-escape (quotes-char)
  "Remove escape when editing docstring.

QUOTES-CHAR should be \" or '."
  (goto-char (point-min))
  (cond ((string= quotes-char "\"")
         (while (re-search-forward "\\\"" nil t)
           (replace-match "")
           (separedit--remove-nested-escape)
           (insert "\"")))
        ((string= quotes-char "'")
         (while (search-forward "\\'" nil t)
           (replace-match "")
           (separedit--remove-nested-escape-sq)
           (insert "'")))))

(defun separedit--restore-nested-escape ()
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

(defun separedit--restore-escape (quotes-char)
  "Restore escape when finished editing docstring.

QUOTES-CHAR should be \" or '."
  (goto-char (point-min))
  (cond ((string= quotes-char "\"")
         (while (search-forward "\"" nil t)
           (replace-match "")
           (separedit--restore-nested-escape)
           (insert "\\\"")))
        ((string= quotes-char "'")
         (while (search-forward "'" nil t)
           (replace-match "")
           (separedit--restore-nested-escape)
           (insert "\\'")))))

(defalias 'separedit--derived-mode-p
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

(defun separedit--select-mode ()
  "Select major mode."
  (completing-read
   "Select mode: "
   (lambda (string pred action)
     (append separedit--mode-history
             (let ((pred
                    (lambda (sym)
                      (and (funcall pred sym)
                           (or (separedit--derived-mode-p sym 'prog-mode)
                               (memq sym
                                     '(text-mode
                                       gfm-mode
                                       markdown-mode
                                       org-mode
                                       separedit-mode
                                       separedit-single-quote-string-mode
                                       separedit-double-quote-string-mode)))))))
               (complete-with-action action obarray string pred))))
   #'commandp t nil 'separedit--mode-history (or (car separedit--mode-history)
                                                 (format "%s" major-mode))))

(defvar separedit-mode-map (make-sparse-keymap) "Keymap used in comment edit buffer.")

(define-minor-mode separedit-mode
  "Minor mode for enable edit code block in comment.\\{separedit-mode-map}"
  :init-value nil
  :group 'separedit
  :global nil
  :keymap 'separedit-mode-map)

(defvar separedit-double-quote-string-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (separedit--entry-key) #'separedit)
    map)
  "Keymap for `separedit-double-quote-string-mode'.")

(define-generic-mode 'separedit-double-quote-string-mode
  nil nil nil nil
  '((lambda ()
      (modify-syntax-entry ?' "\"")
      (use-local-map separedit-double-quote-string-mode-map)))
  "Major mode for editing double-quoted string.")

(defvar separedit-single-quote-string-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (separedit--entry-key) #'separedit)
    map)
  "Keymap for `separedit-single-quote-string-mode'.")

(define-generic-mode 'separedit-single-quote-string-mode
  nil nil nil nil
  '((lambda ()
      (modify-syntax-entry ?' "\"")
      (use-local-map separedit-single-quote-string-mode-map)))
  "Major mode for editing single-quoted string.")

;;;###autoload
(defun separedit-dwim-described-variable ()
  "Edit value of variable at poin in help/helpful buffer."
  (interactive)
  (-if-let* ((info (pcase major-mode
                     (`help-mode (separedit-help-variable-edit-info))
                     (`helpful-mode (separedit-helpful-variable-edit-info))))
             (region (nth 1 info))
             (edit-indirect-after-creation-hook #'separedit--buffer-creation-setup)
             (point-info (separedit--point-info (car region) (cdr region)))
             (edit-indirect-guess-mode-function
              `(lambda (_bufer _beg _end)
                 (emacs-lisp-mode)
                 (separedit--restore-point ,@point-info)
                 (setq-local separedit--inhibit-read-only t)
                 (setq-local separedit--help-variable-edit-info ',info))))
      (edit-indirect-region (car region) (cdr region) 'display-buffer)
    (user-error "Not at variable value")))

;;;###autoload
(defun separedit-dwim-default (&optional block)
  "Edit comment or docstring or code BLOCK in them.

Normally, the major mode of the edit buffer will be selected automatically,
but users can also manually select it by pressing `C-u \\[separedit]'."
  (interactive)
  (let* ((block (or block (separedit--block-info)))
         (beg (plist-get block :beginning))
         (end (plist-get block :end))
         (point-info (separedit--point-info beg end))
         (lang-mode (plist-get block :lang-mode))
         (strp (plist-get block :string-quotes))
         (indent-length (plist-get block :indent-length))
         (indent-line1 (plist-get block :indent-line1))
         (commentp (not strp))
         (codep (and lang-mode t))
         (cbindent (when (and strp
                              (funcall
                               (-orfn #'not #'string-empty-p)
                               (plist-get (plist-get block :regexps) :body)))
                     (plist-get block :code-block-indent)))
         (delimiter-regexp (concat (if strp "^\s*"
                                     (or (plist-get block :comment-delimiter)
                                         (separedit--comment-delimiter-regexp)))
                                   (plist-get (plist-get block :regexps) :body)))
         (edit-indirect-after-creation-hook #'separedit--buffer-creation-setup))
    (separedit--log "==> block-info: %S" block)
    ;; (separedit--log "==> block: %S" (buffer-substring-no-properties beg end))
    (if block
        (let* ((mode (if current-prefix-arg
                         (intern (separedit--select-mode))
                       (if codep
                           (or lang-mode
                               separedit-code-block-default-mode)
                         (cond ((and (stringp strp) (string= "'" strp)) 'separedit-single-quote-string-mode)
                               ((and (stringp strp) (string= "\"" strp)) 'separedit-double-quote-string-mode)
                               (t separedit-default-mode))))))
          (setq-local edit-indirect-guess-mode-function
                      `(lambda (_parent-buffer _beg _end)
                         (let* ((line-delimiter
                                 (when (or ,codep ,commentp)
                                   (separedit--remove-comment-delimiter ,delimiter-regexp ,cbindent)))
                                (indent-len (when (and ,indent-length (not ,codep) (not ,cbindent))
                                              (- ,indent-length (length line-delimiter)))))
                           (separedit--log "==> block(edit buffer): %S" (buffer-substring-no-properties (point-min) (point-max)))
                           (when ,strp
                             (separedit--log "==> quotes(edit buffer): %S" ,strp)
                             (separedit--remove-escape ,strp))
                           (separedit--log "==> mode(edit buffer): %S" ',mode)
                           (funcall ',mode)
                           (when (and indent-len (>= indent-len 0))
                             (set (make-local-variable 'separedit--indent-line1) ,indent-line1)
                             (set (make-local-variable 'separedit--indent-length) (separedit--remove-string-indent indent-len)))
                           (set (make-local-variable 'separedit-leave-blank-line-in-comment)
                                ,separedit-leave-blank-line-in-comment)
                           (set (make-local-variable 'separedit--line-delimiter) line-delimiter)
                           (set (make-local-variable 'separedit--code-block-p) ,codep)
                           (set (make-local-variable 'edit-indirect-before-commit-hook)
                                (append '((lambda ()
                                            (separedit--restore-string-indent)
                                            (separedit--restore-comment-delimiter)
                                            (when ,strp
                                              (separedit--restore-escape ,strp))))
                                        edit-indirect-before-commit-hook))
                           (separedit--restore-point ,@point-info))))
          (edit-indirect-region beg end 'display-buffer))
      (user-error "Not inside a edit block"))))

(defun separedit-mark-region (beg end &optional edit-buffer-mode)
  "Mark region BEG and END as a block."
  (list :beginning beg
        :end       end
        :lang-mode (or edit-buffer-mode separedit-default-mode)))

;;;###autoload
(defun separedit-dwim (&optional block)
  (interactive)
  (cond
   ((memq major-mode '(help-mode helpful-mode))
    (separedit-dwim-described-variable))
   (t (separedit-dwim-default
       (or block
           ;; minibuffer
           (when (and (minibufferp (current-buffer))
                      (not (separedit--point-at-string)))
             (list :beginning (+ (point-min) (length (minibuffer-prompt)))
                   :end       (point-max)
                   :lang-mode 'emacs-lisp-mode))
           ;; region
           (when (region-active-p)
             (list :beginning (region-beginning)
                   :end (if (and (= ?\n (char-before (region-end)))
                                 (not (= ?\n (char-after (region-end)))))
                            (1- (region-end))
                          (region-end))
                   :lang-mode (if (called-interactively-p 'any)
                                  (intern (separedit--select-mode))
                                separedit-default-mode))))))))

;;;###autoload
(defalias 'separedit 'separedit-dwim)

(provide 'separedit)

;;; separedit.el ends here
