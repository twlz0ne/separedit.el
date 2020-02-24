<!-- This file was generated from elisp commentary section by tool, DO NOT EDIT -->

[![Build Status](https://travis-ci.com/twlz0ne/separedit.el.svg?branch=master)](https://travis-ci.com/twlz0ne/separedit.el)

# separedit.el

Edit comment or docstring or code block inside them with your favorite mode.

    +----------+         Edit           +-----------+         Edit           +-----------+
    |          | ---------------------> |   edit    | ---------------------> |   edit    | ...
    |          |  point-at-comment?     |   buffer  |  point-at-comment?     |   buffer  |
    |  source  |  point-at-string?      |           |  point-at-string?      |           | ...
    |  buffer  |  point-at-codeblock?   | (markdown |  point-at-codeblock?   | (markdown | ...
    |          |                        |  orgmode  |                        |  orgmode  |
    |          | <--------------------- |   ...)    | <--------------------- |   ...)    | ...
    +----------+     Commit changes     +-----------+     Commit changes     +-----------+

## Installation

Clone this repository to `~/.emacs.d/site-lisp/separedit`.  Add the following to your `.emacs`:

```elisp
(require 'separedit)
(define-key prog-mode-map (kbd "C-c '") #'separedit)
(setq separedit-default-mode 'markdown-mode) ;; or org-mode
```

## Usage

- Move the cursor to a comment or string, or a code block inside them.
- <kbd>C-c '</kbd>.

    or press <kbd>C-u C-c '</kbd> to starting edit with manually selected major mode.

## Edit comment

`separedit` use **continuity** as basis for determing whether it is a comment **block** or **line**.
Continuous means that there is no barrier (e.g. code or blank line) between the end of previous line and the beginning of next line, for example:

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

By setting `separedit-default-mode` to choose the mode (e.g. `markdown-mode` or `org-mode`) for edit buffer.
In edit buffer, the comment delimiter will be removed, for example:

    source buffer     ->    edit buffer   ->    edit buffer

    /*
     * # Example            # Example
     *
     * ``` C                ``` C
     * foo("bar");          foo("bar");         foo("bar");
     * ```                  ```
     */

    // * Example            * Example
    //
    // #+BEGIN_SRC C        #+BEGIN_SRC C
    // foo("bar");          foo("bar");         foo("bar");
    // #+END_SRC            #+END_SRC

## Edit string

`separedit` provides convenience for editing escaped strings, if there are nested string or code block, just continue press <kbd>C-c '</kbd> to enter a new edit buffer:

    source buffer     ->    edit buffer   ->    edit buffer

    "a\"b\\\"c\\\"\""       a"b\"c\""           b"c"

## Edit code block

`separedit` also support for editing code block directly in comment or string:

    source buffer     ->    eidt buffer

    ",--- elisp
     | (foo \"bar\")        (foo "bar")
     `---"

    /*
     * ``` C
     * foo("bar");          foo("bar");
     * ```
     */

If the language identifier of code block is omitted, the edit buffer uses the same mode as the source buffer.

## Screencasts

<p float="left" align="center">
  <img src="images/separedit1.gif" />
  <img src="images/separedit2.gif" />
</p>

<i>P.S.</i> The language identifier of code block can be omitted in these cases.
