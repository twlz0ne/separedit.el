<!-- This file was generated from elisp commentary section by tool, DO NOT EDIT -->

[![CI](https://github.com/twlz0ne/separedit.el/workflows/CI/badge.svg)](https://github.com/twlz0ne/separedit.el/actions?query=workflow%3ACI)
[![MELPA](https://melpa.org/packages/separedit-badge.svg)](https://melpa.org/#/separedit)

# separedit.el

Edit comment/string/docstring/code block in separate buffer with your favorite
mode.

    +----------+         Edit           +-----------+         Edit           +-----------+
    |          | ---------------------> |   edit    | ---------------------> |   edit    | ...
    |          |  point-at-comment?     |   buffer  |  point-at-comment?     |   buffer  |
    |  source  |  point-at-string?      |           |  point-at-string?      |           | ...
    |  buffer  |  point-at-codeblock?   | (markdown |  point-at-codeblock?   | (markdown | ...
    |          |  point-at-...?         |  orgmode  |  point-at-...?         |  orgmode  |
    |          | <--------------------- |   ...)    | <--------------------- |   ...)    | ...
    +----------+     Commit changes     +-----------+     Commit changes     +-----------+

- [Installation](#installation)
- [Usage](#usage)
    - [Edit comment](#edit-comment)
    - [Edit string](#edit-string)
    - [Edit code block](#edit-code-block)
    - [Edit heredoc](#edit-heredoc)
    - [Edit C/C++ macro](#edit-cc-macro)
    - [Edit value form of variable in help/helpful buffer](#edit-value-form-of-variable-in-helphelpful-buffer)
    - [Edit minibuffer](#edit-minibuffer)
    - [Edit in vterm](#edit-in-vterm)
- [Customization](#customization)
    - [Change key bindings in edit buffer](#change-key-bindings-in-edit-buffer)
    - [Add support for a new major mode](#add-support-for-a-new-major-mode)
    - [Add support for a new code block](#add-support-for-a-new-code-block)
    - [Preserving indentation of block in string](#preserving-indentation-of-block-in-string)
    - [Continue fill-column width in edit buffer](#continue-fill-column-width-in-edit-buffer)
- [Some extended usage](#some-extended-usage)
    - [Combine multipe adjacent blocks as a single edit block](#combine-multipe-adjacent-blocks-as-a-single-edit-block)
    - [Break long lines in comment](#break-long-lines-in-comment)
    - [Eval multiple-line sexp in comment](#eval-multiple-line-sexp-in-comment)

## Installation

Clone this repository, or install from MELPA. Add the following to your
`.emacs`:

```elisp
(require 'separedit)

;; Key binding for modes you want edit
;; or simply bind ‘global-map’ for all.
(define-key prog-mode-map        (kbd "C-c '") #'separedit)
(define-key minibuffer-local-map (kbd "C-c '") #'separedit)
(define-key help-mode-map        (kbd "C-c '") #'separedit)
(define-key helpful-mode-map     (kbd "C-c '") #'separedit)

;; Default major-mode for edit buffer
;; can also be other mode e.g. ‘org-mode’.
(setq separedit-default-mode 'markdown-mode)

;; Feature options
;; (setq separedit-preserve-string-indentation t)
;; (setq separedit-continue-fill-column t)
;; (setq separedit-write-file-when-execute-save t)
;; (setq separedit-remove-trailing-spaces-in-comment t)
```

## Usage

- Move the cursor to a comment/string/code block or any supported place.
- Press <kbd>C-c '</kbd>.

  or press <kbd>C-u C-c '</kbd> to starting edit with manually selected major
  mode.

Can also press <kbd>C-c '</kbd> on an active region.

Following are default keys in edit buffer:

| Key                | Function                                           | Summary                                                             |
|:-------------------|:---------------------------------------------------|:--------------------------------------------------------------------|
| <kbd>C-c C-c</kbd> | `separedit-commit`                                 | Commit changes and close edit buffer                                |
| <kbd>C-x C-s</kbd> | `separedit-save`                                   | Commit changes (even write source file) without closing edit buffer |
| <kbd>C-c C-k</kbd> | `separedit-abort`                                  | Discard changes and close edit buffer                               |
| <kbd>C-c '</kbd>   | `separedit` or follow the settings of markdown/org | Open a new edit buffer                                              |

### Edit comment

`separedit` use **continuity** as basis for determining whether it is a comment
**block** or **line**. Continuous means that there is no barrier (e.g. code or
blank line) between the end of previous line and the beginning of next line, for
example:

    /*
     * this is a
     * comment block
     */

    //
    // this is also a
    // comment block
    //

    //
    // this is another
    // comment block
    //

    code 1 /* all this are comment lines */
    code 2 /* all this are comment lines */
    code 3 // all this are comment lines
    code 4 // all this are comment lines

By setting `separedit-default-mode` to choose the mode (e.g. `markdown-mode` or
`org-mode`) for edit buffer. In edit buffer, the comment delimiter will be
removed, for example (█ represents the cursor):

    source buffer     ->    edit buffer   ->    edit buffer

    /*
     * # Example█           # Example
     *
     * ``` C                ``` C
     * foo("bar");          foo("bar");█        foo("bar");
     * ```                  ```
     */

    // * Example█           * Example
    //
    // #+BEGIN_SRC C        #+BEGIN_SRC C
    // foo("bar");          foo("bar");█        foo("bar");
    // #+END_SRC            #+END_SRC

### Edit string

`separedit` provides convenience for editing escaped strings, if there are
nested string or code block, just continue press <kbd>C-c '</kbd> to enter a new
edit buffer:

    source buffer     ->    edit buffer   ->    edit buffer

    "a█\"b\\\"c\\\"\""       a"b█\"c\""           b"c"

### Edit code block

`separedit` also support for editing code block directly in comment or string:

    source buffer     ->    edit buffer

    ",--- elisp
     | (foo \"bar\")█       (foo "bar")
     `---"

    /*
     * ``` C
     * foo("bar");█         foo("bar");
     * ```
     */

If the language identifier of code block is omitted, the edit buffer uses the
same mode as the source buffer.

### Edit heredoc

The heredoc marker can be used to specify the language:

    source buffer       ->      edit buffer (css-mode)

    ...<<CSS
    h1 {                        h1 {
      color: red;█                color: red;█
    }                           }
    CSS

Both `LANG` and `__LANG__` are supported, see
`separedit-heredoc-language-regexp-alist` for more detail.

### Edit C/C++ macro

    #define█FOO(a, b)    \      ->      #define█FOO(a, b)
    do {                 \              do {
        auto _a = (a);   \                  auto _a = (a);
        auto _b = (b);   \                  auto _b = (b);
    } while (false)                     } while (false)

### Edit value form of variable in help/helpful buffer

Describe a variable, move cursor to the local/global value form, press <kbd>C-c
'</kbd> to edit it.

### Edit minibuffer

Don't get stuck in minibuffer, press <kbd>C-c '</kbd> to open a edit buffer.

### Edit in vterm

Make sure the the vterm
[Directory tracking and Prompt tracking](https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking)
is set correctly.

Then put the cursor after prompt, press <kbd>C-c '</kbd> to start a new edit, or
<kbd>C-p C-c '</kbd> to edit previous command.

## Customization

### Change key bindings in edit buffer

If you don't like the default key bindings in edit buffer, you can change it:

- `separedit-save-key`
- `separedit-entry-key`
- `separedit-abort-key`
- `separedit-commit-key`

### Add support for a new major mode

1. Add the start/end delimiter of block style comment to
   `separedit-comment-encloser-alist`.
2. Add the delimiter of each comment line to
   `separedit-comment-delimiter-alist`.
3. Add the string (including docstring) quotes to
   `separedit-string-quotes-alist`.
4. Add definition to `separedit-string-indent-offset-alist` if there is base
   indent offset in docstring.
5. Add a mode name to `separedit-not-support-docstring-modes` if not support
   docstring.

### Add support for a new code block

1. Add a set of regexps matching the new code block to
   `separedit-block-regexp-plists`.
2. Add a language name to `separedit-code-lang-modes` if can't get mode by
   simply adding suffix `-mode`.

### Preserving indentation of block in string

If `separedit-preserve-string-indentation` is non-nil, the indentation of string
block will be preseved in edit buffer, e.g:

```
source buffer                         edit buffer
+--------------------+                +--------------------+
| def func():        |                | Usage:             |
|     '''            |                |     func()         |
|     Usage:         |       ->       |                    |
|         func()     |                |                    |
|     '''            |                |                    |
|     pass           |                |                    |
+====================+                +====================+
```

No only for the docsting, normal string are also supported:

```
source buffer                         edit buffer
+--------------------+                +--------------------+
| emacs \            |                | (progn             |
|    --batch \       |                |   ...)             |
|    --eval "(progn  |       ->       |                    |
|              ...)" |                |                    |
|                    |                |                    |
+====================+                +====================+
```

### Continue fill-column width in edit buffer

If `separedit-continue-fill-column` is non-nil, use the remaining fill-width in
edit buffer:

```
source buffer                   edit buffer

    //
    // this is a                this is a
    // comment block            comment block
    //

|<---->|<------------>|         |<------------->|
   |         |                         |
   |         '-- available width for --'
   |                edit buffer
used width
```

You may also like to enable `auto-fill-mode` in edit buffer:

```elisp
(add-hook 'separedit-buffer-creation-hook #'auto-fill-mode)
```

## Some extended usage

### Combine multipe adjacent blocks as a single edit block

```elisp
(defun separedit//region-of-el-commentary ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^;;; Commentary:\n+")
      (let ((begin (point)))
        (when (re-search-forward  "\n;;; .*$" nil t)
          (goto-char (match-beginning 0))
          (list begin (point)))))))

(defun separedit/edit-el-commentary ()
  "Edit whole commentary section as a single block."
  (interactive)
  (let ((separedit-leave-blank-line-in-comment t))
    (separedit-dwim
     (apply #'separedit-mark-region
            `(,@(separedit//region-of-el-commentary)
              markdown-mode)))))
```

### Break long lines in comment

```elisp
(defun separedit/re-fill ()
  (interactive)
  (let ((separedit-continue-fill-column t))
    (with-current-buffer (separedit-dwim)
      (fill-region (point-min) (point-max))
      (execute-kbd-macro (kbd "C-c C-c")))))
```

### Eval multiple-line sexp in comment

```elisp
(defun separedit/eval-last-sexp-in-comment ()
  (interactive)
  (let ((separedit-default-mode 'emacs-lisp-mode)
        (separedit-inhibit-edit-window-p t))
    (with-current-buffer (separedit)
      (unwind-protect (call-interactively #'eval-last-sexp)
        (separedit-abort)))))

(define-key emacs-lisp-mode-map (kbd "C-x C-e")
  (lambda ()
    (interactive)
    (call-interactively
     (if (separedit--point-at-comment)
         #'separedit/eval-last-sexp-in-comment
       #'eval-last-sexp))))
```
