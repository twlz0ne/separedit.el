;;; separedit.el --- Edit comment/string/docstring/code block in separate buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/06
;; Version: 0.3.33
;; Package-Requires: ((emacs "25.1") (dash "2.18") (edit-indirect "0.1.5"))
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

;; [![CI](https://github.com/twlz0ne/separedit.el/workflows/CI/badge.svg)](https://github.com/twlz0ne/separedit.el/actions?query=workflow%3ACI)
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

;; {{TOC}}

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

;; ### Edit heredoc

;; The heredoc marker can be used to specify the language:

;;     source buffer       ->      edit buffer (css-mode)

;;     ...<<CSS
;;     h1 {                        h1 {
;;       color: red;█                color: red;█
;;     }                           }
;;     CSS

;; Both `LANG` and `__LANG__` are supported, see `separedit-heredoc-language-regexp-alist` for more detail.

;; ### Edit C/C++ macro

;;     #define█FOO(a, b)    \      ->      #define█FOO(a, b)
;;     do {                 \              do {
;;         auto _a = (a);   \                  auto _a = (a);
;;         auto _b = (b);   \                  auto _b = (b);
;;     } while (false)                     } while (false)

;; ### Edit value form of variable in help/helpful buffer

;; Describe a variable, move cursor to the local/global value form, press <kbd>C-c '</kbd> to edit it.

;; ### Edit minibuffer

;; Don't get stuck in minibuffer, press <kbd>C-c '</kbd> to open a edit buffer.

;; ### Edit in vterm

;; Make sure the the vterm [Directory tracking and Prompt tracking](https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking) is set correctolly.

;; Then put the cursor after prompt, press <kbd>C-c '</kbd> to start a new edit, or <kbd>C-p C-c '</kbd> to edit previous command.

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
;;       (execute-kbd-macro (kbd "C-c C-c")))))
;; ```

;; ### Eval multiple-line sexp in comment

;; ``` elisp
;; (defun separedit/eval-last-sexp-in-comment ()
;;   (interactive)
;;   (let ((separedit-default-mode 'emacs-lisp-mode)
;;         (separedit-inhibit-edit-window-p t))
;;     (with-current-buffer (separedit)
;;       (unwind-protect (call-interactively #'eval-last-sexp)
;;         (separedit-abort)))))

;; (define-key emacs-lisp-mode-map (kbd "C-x C-e")
;;   (lambda ()
;;     (interactive)
;;     (call-interactively
;;      (if (separedit--point-at-comment)
;;          #'separedit/eval-last-sexp-in-comment
;;        #'eval-last-sexp))))
;; ```

;;; Code:

(require 'cl-lib)
(require 'dash)
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
  '((("//+!\\(?:<\\)?" "//+\\(?:<\\)?" "\\*+") . (c-mode
                          c++-mode
                          csharp-mode
                          css-mode
                          go-mode
                          java-mode
                          js-mode
                          objc-mode
                          php-mode
                          rust-mode
                          rustic-mode
                          swift-mode
                          typescript-mode))
    (separedit--web-mode-comment-delimiter-regexp . web-mode)
    (("--")            . (applescript-mode haskell-mode lua-mode))
    (("//+")           . (pascal-mode fsharp-mode))
    ((";+\\(?:###autoload\\)?") . (emacs-lisp-mode
                                   lisp-interaction-mode))
    ((";+")            . (lisp-mode
                          racket-mode
                          scheme-mode
                          fennel-mode))
    (("#+")            . (conf-mode nix-mode python-mode ruby-mode yaml-mode)))
  "Alist of comment delimiter regexp.

Each element should be in one of the following forms:

   - (FUNCTION    . MODE-OR-MODE-LIST)
   - (STRING-LIST . MODE-OR-MODE-LIST)"
  :group 'separedit
  :type 'alist)

(defcustom separedit-comment-encloser-alist
  '((("/\\*+\\(?:!\\)?" "\\*+/") . (c-mode
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
    (separedit--get-web-mode-comment-encloser . web-mode)
    (("{-" "-}")       . haskell-mode)
    (("\\(?:(\\*+\\|{\\**\\)" "\\(?:\\*+)\\|\\**}\\)") . pascal-mode)
    (("(\\*" "\\*)")   . (applescript-mode fsharp-mode ocaml-mode))
    (("#|" "|#")       . (lisp-mode racket-mode scheme-mode))
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
     :edit-mode emacs-lisp-mode)

    (:header "@\\(?:swagger\\|openapi\\)$"
     :body   ""
     :footer ".*\\'"
     :keep-footer t
     :edit-mode yaml-mode)

    (:header "#define[\s\t]+\\([^)]+)\\)[\s\t]*\\\\[\s\t]*$"
     :body   "[\s\t]*\\\\[\s\t]*"
     :footer "[^\\\n]+$"
     :modes (c-mode c++-mode)
     :delimiter-remove-fn separedit--remove-c/c++-macro-delimiter
     :delimiter-restore-fn separedit--restore-c/c++-macro-delimiter
     :straight t
     :keep-header t
     :keep-footer t))
  "Lists of regexp to match code block.

Each element of it is in the form of:

    (:header      REGEX ;; to match the header    line of block
     :body        REGEX ;; to match the each body line of block
     :footer      REGEX ;; to match the footer    line of block
     :straight    BOOL  ;; block not in string and comment (optional)
     :delimiter-remove-fn FN ;; function to remove delimiter (optional)
     :delimiter-restore-fn FN ;; function to restore delimiter (optional)
     :keep-footer BOOL  ;; display the footer in edit buffer (optional)
     :keep-footer BOOL  ;; display the footer in edit buffer (optional)
     :modes       MODES ;; major modes the regex apply to (optional)
     :edit-mode   MODE) ;; major mode for edit buffer (optional)"
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

(defcustom separedit-before-abort-hook nil
  "Functions called before abort edit."
  :group 'separedit
  :type 'hook)

(defvar separedit-inhibit-edit-window-p nil
  "Non-nil for open the edit buffer in background.

For example:

   (let ((separedit-inhibit-edit-window-p t))
     (with-current-buffer (separedi)
       (unwind-protect
           (progn
             ;; DO SOMETHING
             )
         (separedit-abort))))")

(defvar separedit-replace-match-function nil
  "Function to instead of `replace-match' when commit changes.")

(defvar separedit-shebang-regexp "#!\\(?:/usr\\)?/bin/"
  "Regexp to match shebang.")

(defvar separedit-heredoc-endwith-trailing-newline-modes
  '(sh-mode perl-mode ruby-mode racket-mode)
  "List of mode that the heredoc end with a trailing newline.

For example (`|' represents the cursor):

    cat <<EOF                            echo <<<EOF
    end with a trailing newline    vs    end with a trailing semicolon
    EOF                                  EOF|;
    |")

(defvar separedit-perl-heredoc-lookback-regexp
  (rx (seq "<<"
           (opt (any "\"'"))
           (group (zero-or-more "_")
                  (group (one-or-more alnum))
                  (zero-or-more "_"))
           (opt (any "\"'"))
           ";" point))
  "Regexp to match the header of heredoc before point.")

(defvar separedit-heredoc-language-regexp-alist
  '((perl-mode    . "<<['\"]?_*\\([[:alnum:]]+\\)_*[\"']?;")
    (php-mode     . "<<<['\"]?_*\\([[:alnum:]]+\\)_*[\"']?")
    (racket-mode  . "#<<['\"]?_*\\([[:alnum:]]+\\)_*[\"']?")
    (ruby-mode    . "<<[-~]?['\"]?_*\\([[:alnum:]]+\\)_*[\"']?\\(?:\\..*\\)?")
    (sh-mode      . "<<-?\s*['\"]?_*\\([[:alnum:]]+\\)_*[\"']?\\(?:.*\\)?")
    (tuareg-mode  . "{_*\\([[:alnum:]]+\\)_*|"))
  "Alist of (MAJOR-MODE . REGEXP) to capture the language name in the begin
marker of heredoc before point.")

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

(defun separedit--get-mode-comment (mode symbol)
  "Get comment of MODE by SYMBOL."
  (or (get mode symbol)
      (if (equal mode major-mode)
          (put mode symbol (symbol-value symbol))
        (with-temp-buffer
          (funcall mode)
          (put mode 'comment-start      comment-start)
          (put mode 'comment-start-skip comment-start-skip)
          (put mode 'comment-end        comment-end)
          (put mode 'comment-end-skip   comment-end-skip)
          (get mode symbol)))))

;;; String / Docstring functions

(defcustom separedit-string-quotes-alist
  '((python-mode     . ("\"\"\"" "'''" "\"" "'"))
    (js-mode         . ("\"" "'" "`"))
    (lua-mode        . ("\"" "'" (or (seq "[" (zero-or-more "=") "[")
                                     (seq "]" (zero-or-more "=") "]"))))
    (nix-mode        . ("''" "\""))
    (typescript-mode . ("\"" "'" "`"))
    (separedit-double-quote-string-mode . t)
    (separedit-single-quote-string-mode . ("'"))
    (sh-mode         . ("'" "\""))
    (fish-mode       . ("'" "\""))
    (t               . ("\"")))
  "Alist of string quotes.

Each item may be one of the following forms:

- Specific mode:        (MAJOR-MODE . (A-LIST-OF-STRING-OR-RX-FORM))
- Default:              (t          . (A-LIST-OF-STRING-OR-RX-FORM))
- Reference default:    (MAJOR-MODE . t)"
  :group 'separedit
  :type 'alist)

(defun separedit--point-at-string (&optional pos)
  "Determine if point POS at string or not."
  (or (nth 3 (syntax-ppss pos))
      (and (<= 29 emacs-major-version)
           (derived-mode-p 'python-mode)
           (memq (face-at-point) '(font-lock-string-face))
           (memq (get-char-property (1- (point)) 'face) '(font-lock-string-face)))
      (and (derived-mode-p 'perl-mode)
           (memq (face-at-point) '(perl-heredoc))
           (not (or (separedit--string-quotes pos)
                    (separedit--string-quotes pos t))))))

(defun separedit--string-beginning (&optional disregard-heredoc-p)
  "Backward to the beginning of string and return the point."
  (while (separedit--point-at-string) (backward-char))
  (unless disregard-heredoc-p
    (unless (separedit--string-quotes (point))
      (forward-line)))
  (point))

(defun separedit--string-end (&optional disregard-heredoc-p)
  "Forward to the end of string and return the point."
  (while (separedit--point-at-string) (forward-char))
  (unless (or disregard-heredoc-p (separedit--string-quotes (1- (point))))
    (unless (separedit--string-quotes (point) t)
      (if (and (apply #'derived-mode-p separedit-heredoc-endwith-trailing-newline-modes)
               (if (eobp) (eq ?\n (char-before)) t))
          (forward-line -2)
        (forward-line -1))
      (goto-char (point-at-eol))))
  (point))

(defun separedit--string-quotes (pos &optional backwardp mode)
  "Return quote characters around string at POS.

If BACKWARDP is not nil, search backward.
If MODE is nil, use ‘major-mode’."
  (let* ((mode (or mode major-mode))
         (looking-fn (if backwardp 'looking-back 'looking-at))
         (quotes (separedit-get-mode-quotes mode))
         (regexp (when quotes
                   (rx-to-string `(or ,@(separedit-get-mode-quotes mode))))))
    (save-excursion
      (when pos
        (goto-char pos))
      (when (and regexp (funcall looking-fn regexp))
        (match-string 0)))))

(cl-defgeneric separedit--string-region (&optional pos disregard-heredoc-p)
  "Return the region of string at point POS.

If DISREGARD-HEREDOC-P is non-nil, don't exclude the heredoc markers.")

(cl-defmethod separedit--string-region (&optional pos disregard-heredoc-p)
  "Return the region of string at point POS.

If DISREGARD-HEREDOC-P is non-nil, don't exclude the heredoc markers."
  (save-excursion
    (when pos
      (goto-char pos))
    (if (separedit--point-at-string)
        (let* ((fbeg (save-excursion
                       (separedit--string-beginning disregard-heredoc-p)))
               (fend (save-excursion
                       (separedit--string-end disregard-heredoc-p)))
               (quotes-len (length (separedit--string-quotes fbeg))))
          (list (+ (or fbeg (point-min)) quotes-len)
                (- (or fend (point-max)) quotes-len)))
      (user-error "Not inside a string"))))

(cl-defmethod separedit--string-region (&context (major-mode nix-mode)
                                        &optional pos _)
  "Return the region of nix string at point POS.

According to the desgin (see https://github.com/NixOS/nix-mode/issues/96), the
string can not be determined when the cursor is on the interpolated variable,
for example:

    (with-temp-buffer
      (insert \"''foo ${bar} quux''\")
      (nix-mode)
      (font-lock-ensure)
      (goto-char (length \"''foo ${b\"))
      (nth 3 (syntax-ppss)))
    ;; => nil"
  (save-excursion
    (when pos
      (goto-char pos))
    (-if-let* ((beg
                (catch 'beg
                  (save-excursion
                    (while (re-search-backward "\\(\"\\|''\\)" nil t)
                      (unless (or (separedit--point-at-string (1- (point)))
                                  (looking-back "}" nil))
                        (throw 'beg (match-end 1)))))))
               (end
                (catch 'end
                  (save-excursion
                    (while (re-search-forward "\\(\"\\|''\\)" nil t)
                      (unless (or (separedit--point-at-string)
                                  (looking-at-p "\\${"))
                        (throw 'end (match-beginning 1))))))))
        (list beg end)
      (user-error "Not inside a string"))))

;;; Comment functions

(defvar separedit-comment-faces '(font-lock-comment-face
                                  font-lock-comment-delimiter-face
                                  typescript-jsdoc-tag
                                  typescript-jsdoc-type
                                  typescript-jsdoc-value
                                  web-mode-block-comment-face
                                  web-mode-comment-face
                                  web-mode-css-comment-face
                                  web-mode-javascript-comment-face
                                  web-mode-json-comment-face
                                  web-mode-part-comment-face)
  "List of comment face.")

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
    (pcase (car def)
      ((and fn (pred functionp)) (funcall fn))
      ((and strs (pred consp)) (concat "^[\s\t]*\\(?:"
                                       (mapconcat #'identity strs "\\|")
                                       "\\)\s?"))
      (_ (concat "^[\s\t]*\\(?:"
                 (separedit--get-mode-comment mode 'comment-start-skip)
                 "\\)\s?")))))

(defun separedit--web-mode-comment-delimiter-regexp ()
  "Return web-mode comment delimiter regex."
  (pcase (bound-and-true-p web-mode-engine)
    ("none" (pcase (bound-and-true-p web-mode-content-type)
              ((or "html" "xml")
               (separedit--comment-delimiter-regexp 'html-mode))
              ((or "css" "javascript" "jsx" "typescript")
               (separedit--comment-delimiter-regexp 'js-mode))))
    ("angular" (separedit--comment-delimiter-regexp 'js-mode))
    ("aspx"    (separedit--comment-delimiter-regexp 'js-mode))
    ("blade"   (separedit--comment-delimiter-regexp 'php-mode))
    ("django"  (separedit--comment-delimiter-regexp 'python-mode))
    ("erb"     (separedit--comment-delimiter-regexp 'ruby-mode))
    ("go"      (separedit--comment-delimiter-regexp 'go-mode))
    ("jsp"     (separedit--comment-delimiter-regexp 'java-mode))
    ("php"     (separedit--comment-delimiter-regexp 'php-mode))))

(defun separedit--point-at-comment-exclusive-one-line ()
  "Determine if comment exclusive one line and return the comment face."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (goto-char (point-min))
      (and (re-search-forward "[^\s\t]" nil t 1)
           (and (ignore-errors (forward-char 1) t)
                (separedit--point-at-comment))))))

(defun separedit--comment-faces ()
  "Return comment faces of current mode."
  (append separedit-comment-faces
          (when (apply #'derived-mode-p separedit-not-support-docstring-modes)
            '(font-lock-doc-face))))

(defun separedit--point-at-comment (&optional point)
  "Return non-nil (face or t) if POINT at comment."
  (let ((face (get-text-property (or point (if (and (eobp) (not (bolp)))
                                               (1- (point))
                                             (point)))
                                 'face)))
    (when face
      (or (let ((comment-faces (separedit--comment-faces)))
            (cl-loop for f in (if (consp face) (reverse face) (list face))
                     when (or (memq f comment-faces)
                              (memq (face-attribute f :inherit) comment-faces))
                     return f))
          (when (or (and (bound-and-true-p whitespace-mode)
                         (string-prefix-p "whitespace-"
                                          (symbol-name 'whitespace-space)))
                    (and (derived-mode-p 'sh-mode)
                         ;; Last word of shebang has different face
                         (= (point-min) (point-at-bol))))
            (save-excursion
              (let ((state (syntax-ppss)))
                (and (nth 4 state)
                     (parse-partial-sexp (point) (point-max)
                                         nil nil state 'syntax-table)
                     t))))))))

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
    (when (and (bobp) (looking-at-p separedit-shebang-regexp))
      (forward-line 1))
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

(cl-defgeneric separedit--comment-region (&optional pos)
  "Return the region of continuous comments at POS.")

(cl-defmethod separedit--comment-region (&optional pos)
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

(cl-defmethod separedit--comment-region (&context (major-mode perl-mode)
                                         &optional pos)
  "Fix heredoc region detection of perl-mode for Emacs 27 and lower."
  (let ((region (cl-call-next-method pos))
        heredoc-mark)
    (if (and (<= emacs-major-version 27)
             (derived-mode-p 'perl-mode)
             (save-excursion
               (goto-char (car region))
               (when (re-search-backward
                      separedit-perl-heredoc-lookback-regexp nil t)
                 (setq heredoc-mark (match-string 1)))))
        (list (1+ (car region))
              (- (cadr region) (1+ (length heredoc-mark))))
      region)))

(defun separedit--skip-comment-begin-encloser (&optional multi-line-p mode)
  "Search forward from point for the beginning encloser of comment.

MULTI-LINE-P means whether the comment is multi-line.
If MODE is nil, use ‘major-mode’."
  (re-search-forward
   (concat
    (car (separedit--get-comment-encloser (or mode major-mode)))
    "[\t\s]*"
    (when multi-line-p
      "\n?"))))

(defun separedit--skip-comment-end-encloser (&optional multi-line-p mode)
  "Search backward from point for the beginning encloser of comment.

MULTI-LINE-P means whether the comment is multi-line.
If MODE is nil, use ‘major-mode’."
  (let ((encloser-end
         (cadr (separedit--get-comment-encloser (or mode major-mode)))))
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
  "Return comment ‘(begin-encloser end-encloser)’ for MODE."
  (let* ((mode (or mode major-mode))
         (def (or (separedit--rassoc mode separedit-comment-encloser-alist)
                  (separedit--rassoc (get mode 'derived-mode-parent)
                                     separedit-comment-encloser-alist)
                  (separedit--rassoc (separedit--get-real-mode mode)
                                     separedit-comment-encloser-alist))))
    (if def
        (if (functionp (car def))
            (funcall (car def))
          (car def))
      (unless (string-empty-p (separedit--get-mode-comment mode 'comment-end))
        (list (separedit--get-mode-comment mode 'comment-start-skip)
              (or (separedit--get-mode-comment mode 'comment-end-skip)
                  (separedit--get-mode-comment mode 'comment-end)))))))

(defun separedit--get-web-mode-comment-encloser ()
  "Return web-mode comment encloser."
  (pcase (bound-and-true-p web-mode-engine)
    ("none" (pcase (bound-and-true-p web-mode-content-type)
              ((or "html" "xml")
               (separedit--get-comment-encloser 'html-mode))
              ((or "css" "javascript" "jsx" "typescript")
               (separedit--get-comment-encloser 'js-mode))))
    ("angular" (separedit--get-comment-encloser 'js-mode))
    ("aspx"   '("<%--\\(?:\s+\\|\s*$\\)"          "\\(?:^\s*\\|\s\\)--%>"))
    ("blade"  '("{{--\\(?:\s+\\|\s*$\\)"          "\\(?:^\s*\\|\s\\)--}}"))
    ("django" '("{#\\(?:\s+\\|\s*$\\)"            "\\(?:^\s*\\|\s\\)#}"))
    ("erb"    '("<%#\\(?:\s+\\|\s*$\\)"           "\\(?:^\s*\\|\s\\)%>"))
    ("go"     '("{{/\\*+\\(?:\s+\\|\s*$\\)"       "\\(?:^\s*\\|\s\\)\\*}}"))
    ("jsp"    '("<%--\\(?:\s+\\|\s*$\\)"          "\\(?:^\s*\\|\s\\)--%>"))
    ("php"    '("<\\?php /\\*+\\(?:\s+\\|\s*$\\)" "\\(?:^\s*\\|\s\\)\\*\\?>"))))

(defun separedit--enclosed-comment-p (&optional comment-beginning comment-close)
  "Determine if the comment from COMMENT-BEGINNING to COMMENT-CLOSE is enclosed."
  (-when-let (encloser (separedit--get-comment-encloser major-mode))
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

(defun separedit--code-block-beginning (&optional comment-delimiter block-regexp-plists)
  "Return code block info containing ‘:beginning’.

Search process will skip characters COMMENT-DELIMITER at beginning of each line.
If BLOCK-REGEXP-PLISTS non-nil, use it instead of `separedit-block-regexp-plists'."
  (let* ((block-regexp-plists
          (or block-regexp-plists
              (--filter
               (let ((modes (plist-get it :modes)))
                 (when (or (not modes) (apply #'derived-mode-p modes))
                   it))
               separedit-block-regexp-plists)))
         (regexp-group
          (concat
           comment-delimiter
           "\\(\s*\\)" ;; group 1
           "\\("       ;; group 2,3
           (mapconcat
            (lambda (it)
              (plist-get it :header))
            block-regexp-plists
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
                                  block-regexp-plists)))))
                   (list
                    :indent-length
                    (when (and
                           (funcall (-orfn #'not #'string-empty-p) comment-delimiter)
                           (funcall (-orfn #'not #'string-empty-p) (plist-get block-regexp :body)))
                      code-block-indent)
                    :beginning (progn
                                 (unless (plist-get block-regexp :keep-header)
                                   (separedit--beginning-of-next-line))
                                 (point-at-bol))
                    :lang-mode
                    (or (plist-get block-regexp :edit-mode)
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
      (let* ((code-pl (plist-get code-info :regexps))
             (keep-footer-p (plist-get code-pl :keep-footer))
             (regexp (concat (plist-get code-info :comment-delimiter)
                             (plist-get code-pl :footer))))
        (separedit--log "==> [code=block-end] regexp: %s" regexp)
        (when (re-search-forward regexp nil t)
          (unless keep-footer-p
            (separedit--end-of-previous-line))
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

(defun separedit-looking-back-heredoc-language (&optional mode)
  "Return languge name in heredoc start marker before point."
  (let* ((mode (or mode major-mode))
         (regexp (cdr (assoc mode separedit-heredoc-language-regexp-alist))))
    (when (looking-back regexp (line-beginning-position))
      (match-string-no-properties 1))))

(defun separedit--indent-of-string-block (quotes beg end)
  "Return the indent info of string block between BEN and END quoted by QUOTES.
Return value is in the form of (indent-length indent-line1)."
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
             (cons nil nil))
            ((and (string= str-start "") beg-at-newline end-at-newline)
             ;; For
             ;;
             ;;   '''
             ;;   string block need preserve indetation
             ;;   '''
             (cons (- (current-column) (length quotes)) t))
            ((and (string= str-start "") (not beg-at-newline) end-at-newline)
             ;; For
             ;;
             ;;   str = '''
             ;;   string block need preserve indetation
             ;;   '''
             (goto-char end)
             (cons
              (+ (current-column)
                 (or (cdr (assoc major-mode separedit-string-indent-offset-alist))
                     0))
              t))
            ((not (string= str-start ""))
             ;; For situations like:
             ;;
             ;;   emacs --batch --eval "(progn
             ;;                           ...)"
             ;;
             (cons (current-column) nil))))))

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
      :indent-length nil)

:regexps        not nil means point at a code block, for more information
                see `separedit-block-regexp-plists'.
:string-quotes  not nil means point at a string block otherwise a comment block.
:indent-length  base indent of comment/string/code block

                string:

                        ''|indent base
                          rest string''

                        ''
                        |indent base
                        rest string
                        ''

                comment:

                        /* |indent base
                           rest comment */

                        /*
                         *|indent base
                         * rest comment
                         */

                code block:

                        ```
                        |indent base
                        rest code
                        ```"
  (let* ((pos (point))
         (strp nil)
         (straight-block nil)
         (comment-or-string-region
          ;; string
          (if (separedit--point-at-comment)
              (when (or (derived-mode-p 'prog-mode 'conf-mode)
                        (memq major-mode '(yaml-mode gfm-mode markdown-mode org-mode)))
                (condition-case nil (separedit--comment-region) (user-error nil)))
            (let ((region (condition-case nil (separedit--string-region) (user-error nil))))
              ;; comment
              (if region
                  (prog1 region
                    (setq strp (separedit--string-quotes
                                (save-excursion
                                  (goto-char (car region))
                                  (separedit--string-beginning)))))
                ;; straight
                (let ((block-regexps
                       (--filter
                        (let ((modes (plist-get it :modes)))
                          (when (and modes (apply #'derived-mode-p modes))
                            it))
                        separedit-block-regexp-plists)))
                  (when block-regexps
                    (setq straight-block
                          (separedit--code-block-end
                           (save-excursion ;; correct detection of header (e.g.
                             (end-of-line) ;; c/c++ macro) when the point on it.
                             (separedit--code-block-beginning nil block-regexps))))))
                (unless straight-block
                  (user-error "Not inside a edit block"))))))
         (heredoc-lang
          (when comment-or-string-region
            (save-excursion
              (goto-char (car comment-or-string-region))
              (when (and (bolp) (not (bobp)))
                (backward-char))
              (separedit-looking-back-heredoc-language))))
         (heredoc-mode (if heredoc-lang (separedit-get-lang-mode heredoc-lang)))
         (indent-line1 nil)
         (string-indent
          (when comment-or-string-region
            (if (and strp separedit-preserve-string-indentation)
                (let ((indent-info (apply #'separedit--indent-of-string-block
                                          strp
                                          comment-or-string-region)))
                  (setq indent-line1 (cdr indent-info))
                  (car indent-info))
              (save-excursion
                (goto-char (car comment-or-string-region))
                ;; Not at "/*|"
                (unless (= (point) (point-at-eol))
                  (if (= (point) (point-at-bol))
                      ;; At "| * comment"
                      (save-excursion
                        (when (re-search-forward "[^\s\t]" nil t)
                          (setq indent-line1 t)
                          (backward-char)
                          (current-column)))
                    ;; At "/* |comment"
                    (current-column))))))))
    (save-restriction
      (apply #'narrow-to-region (if comment-or-string-region
                                    comment-or-string-region
                                  (if straight-block
                                      (list
                                       (plist-get straight-block :beginning)
                                       (plist-get straight-block :end)))))
      (let* ((code-info
              (if straight-block straight-block
                (separedit--code-block-end
                 (separedit--code-block-beginning
                  (unless strp (separedit--comment-delimiter-regexp))))))
             (indent-length (or (plist-get code-info :indent-length)
                                string-indent)))
        (if (and code-info
                 (<= (plist-get code-info :beginning) pos)
                 (<= pos (plist-get code-info :end)))
            (append code-info (list :string-quotes strp
                                    :indent-line1 indent-line1
                                    :indent-length indent-length))
          (list :beginning (if (and strp (eq (char-after (point-min)) ?\n))
                               (1+ (point-min))
                             (point-min))
                :end (if strp
                         (save-excursion
                           (goto-char (point-max))
                           (when (looking-back "\n\s*" 1)
                             (forward-line -1)
                             (goto-char (point-at-eol)))
                           (point))
                       (point-max))
                :lang-mode heredoc-mode
                :string-quotes (if heredoc-lang "" strp)
                :indent-line1 indent-line1
                :indent-length indent-length))))))

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
               (if (or (and (<= depth 0) (not start))
                       (looking-back "^Value:\\(\n\\|\s\\)" 1))
                   (throw 'break
                     (if (separedit--point-at-string)
                         (let ((region (separedit--string-region)))
                           (list (cons (car region) (cadr region)) 'stringp))
                       (list (bounds-of-thing-at-point 'sexp) nil)))
                 (goto-char start)))))))

(defun separedit-help-variable-edit-info ()
  "Return help varible edit info at point.

Each element is in the form of (SYMBOL VALUE-BOUND STRINGP SCOPE LOCAL-BUFFER)."
  (unless (eq major-mode 'help-mode)
    (user-error "Not in help buffer"))
  (let* ((symbol (separedit-described-symbol))
         (bound (separedit-described-value-bound))
         (scope-buffer
          (save-excursion
            (goto-char (point-min))
            (let ((buffer?
                   (when (re-search-forward
                          "^Local in buffer \\([^;]+\\); global value is[\s]?"
                          nil t 1)
                     (match-string-no-properties 1))))
              (goto-char (caar bound))
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
    (when (and symbol bound (car scope-buffer))
      `(,symbol ,@bound ,@scope-buffer))))

(defun separedit-helpful-variable-edit-info ()
  "Return helpful variable edit info (symbol value-bound type local-buffer) at point."
  (unless (eq major-mode 'helpful-mode)
    (user-error "Not in help buffer"))
  (let* ((symbol (separedit-described-symbol))
         (bound (separedit-described-value-bound))
         (type-buffer
          (save-excursion
            (goto-char (caar bound))
            (forward-line -1)
            (cond ((looking-at "^Value$") (list 'global nil))
                  ((looking-at "^Value in #<buffer \\(.*\\)>$")
                   (list 'local (match-string-no-properties 1)))
                  ((looking-at "^Original Value$") (list 'global nil))))))
    (when (and symbol bound (car type-buffer))
      `(,symbol ,@bound ,@type-buffer))))

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

(defun separedit--commit ()
  "Commit chnages."
  (cl-letf (((symbol-function 'replace-match)
             (symbol-function (or separedit-replace-match-function
                                  'replace-match))))
    (edit-indirect--commit)))

(defun separedit--apply-changes ()
  "Apply changes to source buffer."
  (let ((inhibit-read-only separedit--inhibit-read-only))
    (edit-indirect--barf-if-not-indirect)
    (if separedit--help-variable-edit-info
        (let* ((sym (nth 0 separedit--help-variable-edit-info))
               (scp (nth 3 separedit--help-variable-edit-info))
               (buf (nth 4 separedit--help-variable-edit-info))
               (val (if (eq 'stringp (nth 2 separedit--help-variable-edit-info))
                        (substring-no-properties (buffer-string)) 
                      (list 'quote (car (read-from-string (buffer-string)))))))
          (cond
           ((and (eq scp 'local) buf) (with-current-buffer buf (eval `(setq-local ,sym ,val))))
           ((and (eq scp 'global) (not buf)) (eval `(setq-default ,sym ,val)))
           (t (message "Unknown variable scope: %s" scp)))
          ;; Make sure `edit-indirect--overlay' not be destroyed.
          (when (overlay-buffer edit-indirect--overlay)
            (separedit--commit)))
      (separedit--commit))))

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

(defun separedit-abort ()
  "Abort edit."
  (interactive)
  (run-hooks 'separedit-before-abort-hook)
  (edit-indirect-abort))

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
            (pcase major-mode
              (`markdown-mode #'markdown-edit-code-block)
              (`gfm-mode #'gfm-edit-code-block)
              (`org-mode #'org-edit-special)
              (_ (when (derived-mode-p 'separedit-single-quote-string-mode
                                       'separedit-double-quote-string-mode
                                       'fundamental-mode
                                       'text-mode
                                       'prog-mode)
                   #'separedit))))
      (let ((km (copy-keymap edit-indirect-mode-map)))
        (separedit--log "==> [-buffer-creation-setup] major-mode: %s, entry-cmd: %s" major-mode entry-cmd)
        (define-key km (separedit--entry-key) entry-cmd)
        (define-key km separedit-commit-key #'separedit-commit)
        (define-key km separedit-save-key #'separedit-save)
        (define-key km separedit-abort-key #'separedit-abort)
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
                                     "\\[separedit-abort]: Abort"
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

(defun separedit--remove-c/c++-macro-delimiter (_ &optional _)
  "Remove c/c++ macro delimiter of each line when etering edit buffer."
  (let ((inhibit-read-only t)
        ;; Regexp from `separedit-block-regexp-plists' is not working as 
        ;; expected when search backward, use the following instead:
        (regexp "\\(?:[^\s\t]\\|^[\s\t]\\)\\([\s\t]*\\\\[\s\t]*\\)"))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward regexp nil t)
        (replace-match "" nil nil nil 1)
        (goto-char (1- (point-at-bol)))))))

(defun separedit--restore-c/c++-macro-delimiter (&optional _)
  "Restore c/c++ macro delimiter of each line when returning from edit buffer."
  (save-excursion
    (goto-char (point-min))
    (while (progn
             (goto-char (point-at-eol))
             (not (eobp)))
      (insert " \\")
      (forward-line))
    (c-indent-region (point-min) (point-max))))

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
               (when (and (not separedit--indent-line1)
                          (looking-at-p "[^\s\t\n\r]"))
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
             (point-info
              (if (and (or print-level print-length)
                       (save-excursion
                         (goto-char (cdr region))
                         (looking-back "\\.\\{3\\}\\([)]+\\)?" nil)))
                  (progn
                    (when (yes-or-no-p
                           "Can't edit the value if it is not printed completely.
\nTemporarily turn off the printing limitation and try agian? ")
                      (let (print-level print-length)
                        (pcase major-mode
                          (`help-mode (revert-buffer t t t))
                          (`helpful-mode (helpful-update)))))
                    (signal 'user-error nil))
                (separedit--point-info (car region) (cdr region)))))
      (let* ((strp (and (nth 2 info) t))
             (edit-indirect-after-creation-hook #'separedit--buffer-creation-setup)
             (edit-indirect-guess-mode-function
              `(lambda (_bufer _beg _end)
                 (when ,strp
                   (separedit--remove-escape ,strp))
                 (emacs-lisp-mode)
                 (set (make-local-variable 'edit-indirect-before-commit-hook)
                      (append '((lambda ()
                                  (when ,strp
                                    (separedit--restore-escape ,strp))))
                              edit-indirect-before-commit-hook))
                 (separedit--restore-point ,@point-info)
                 (setq-local separedit--inhibit-read-only t)
                 (setq-local separedit--help-variable-edit-info ',info))))
        (edit-indirect-region (car region) (cdr region) 'display-buffer))
    (user-error "Not at variable value")))

;;; vterm

(declare-function vterm-delete-region "vterm")
(declare-function vterm-insert "vterm")
(declare-function vterm-copy-mode "vterm")
(declare-function vterm--get-prompt-point "vterm")

(defun separedit--vterm-replace-match (newtext &optional _ _ _ _)
  "Insert NEWTEXT to vterm.

This function is used to instead of ‘replace-match’."
  (let ((inhibit-read-only t))
    (when (< (match-beginning 0) (match-end 0))
      (vterm-send "C-a")
      (vterm-send "C-k"))
    (run-with-idle-timer 0 nil #'vterm-insert newtext)))

(defun separedit--vterm-exit-copy-mode ()
  (when edit-indirect--overlay
    (with-current-buffer (overlay-buffer edit-indirect--overlay)
      (vterm-copy-mode -1))))

;;;###autoload
(defun separedit-dwim-vterm ()
  "Edit content after vterm prompt."
  (interactive)
  (let* ((edit-indirect-after-creation-hook #'separedit--buffer-creation-setup)
         (start-point (vterm--get-prompt-point))
         (end-point (save-excursion
                      (goto-char (point-max))
                      (re-search-backward "[^\s\t\r\n]" nil t 1)
                      (max start-point (1+ (point)))))
         (edit-indirect-guess-mode-function
          (lambda (_buffer _beg _end)
            (setq-local separedit--inhibit-read-only t)
            (setq-local edit-indirect--inhibit-read-only t)
            (setq-local separedit-replace-match-function
                        'separedit--vterm-replace-match)
            (setq-local separedit-before-abort-hook
                        (append '(separedit--vterm-exit-copy-mode)
                                separedit-before-abort-hook))
            (setq-local edit-indirect-before-commit-hook
                        (append '(separedit--vterm-exit-copy-mode)
                                edit-indirect-before-commit-hook)))))
    (vterm-copy-mode 1)
    (edit-indirect-region start-point end-point 'display-buffer)))

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
         (straightp (plist-get (plist-get block :regexps) :straight))
         (commentp (and (not strp) (not straightp)))
         (codep (and lang-mode t))
         (indent-length (plist-get block :indent-length))
         (indent-line1 (plist-get block :indent-line1))
         (local-fill-column fill-column)
         (local-tab-width tab-width)
         (delimiter-remove-fn
          (or (plist-get (plist-get block :regexps) :delimiter-remove-fn)
              #'separedit--remove-comment-delimiter))
         (delimiter-restore-fn
          (or (plist-get (plist-get block :regexps) :delimiter-restore-fn)
              #'separedit--restore-comment-delimiter))
         (delimiter-regexp
          (let ((regexp (concat (if strp "^[\s\t]*"
                                  (if commentp
                                      (or (plist-get block :comment-delimiter)
                                          (separedit--comment-delimiter-regexp))))
                                (plist-get (plist-get block :regexps) :body))))
            (replace-regexp-in-string
               "\\(\s+\\)$"
               (lambda (match)
                 (format "\\\\(?:%s\\\\|\\\\)" (match-string 1 match)))
               regexp)))
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
                                 (when (or ,(and strp codep) ,commentp ,straightp)
                                   (funcall ',delimiter-remove-fn ,delimiter-regexp ,(unless commentp indent-length))))
                                (indent-len (when ,indent-length
                                              (- ,indent-length
                                                 (if (stringp line-delimiter)
                                                     (let ((tab-width ,local-tab-width))
                                                       (string-width line-delimiter))
                                                   0)))))
                           (separedit--log "==> block(edit buffer): %S" (buffer-substring-no-properties (point-min) (point-max)))
                           (separedit--log "==> line-delimiter: %S" line-delimiter)
                           (separedit--log "==> indent-len: %s" indent-len)
                           (when ,strp
                             (separedit--log "==> quotes(edit buffer): %S" ,strp)
                             (separedit--remove-escape ,strp))
                           (separedit--log "==> mode(edit buffer): %S" ',mode)
                           (if (and line-delimiter
                                    (memq ',major-mode '(emacs-lisp-mode lisp-interaction-mode))
                                    (string-match-p ";;;###autoload\s*?" line-delimiter))
                               (funcall ',major-mode)
                             (funcall ',mode))
                           (when (and indent-len (>= indent-len 0))
                             (set (make-local-variable 'separedit--indent-line1) ,indent-line1)
                             (set (make-local-variable 'separedit--indent-length) (separedit--remove-string-indent indent-len)))
                           (set (make-local-variable 'separedit-leave-blank-line-in-comment)
                                ,separedit-leave-blank-line-in-comment)
                           (set (make-local-variable 'fill-column) ,local-fill-column)
                           (set (make-local-variable 'separedit--line-delimiter) line-delimiter)
                           (set (make-local-variable 'separedit--code-block-p) ,codep)
                           (set (make-local-variable 'edit-indirect-before-commit-hook)
                                (append '((lambda ()
                                            (separedit--restore-string-indent)
                                            (funcall ',delimiter-restore-fn)
                                            (when ,strp
                                              (separedit--restore-escape ,strp))))
                                        edit-indirect-before-commit-hook))
                           (separedit--restore-point ,@point-info))))
          (edit-indirect-region beg end (unless separedit-inhibit-edit-window-p
                                          'display-buffer)))
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
   ((memq major-mode '(vterm-mode))
    (separedit-dwim-vterm))
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
