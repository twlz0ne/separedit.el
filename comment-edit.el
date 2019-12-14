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

;; * Configuration:


;; ```
;; (require 'comment-edit)
;; (define-key prog-mode-map (kbd "C-c '") #'comment-edit)
;; ```

;; * Usage

;; - Move cursor in comment area
;; - Press <kbd>C-c '</kbd>

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
                          rust-mode
                          swift-mode))
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
    (:beginning ",---+\s?\\(\\w*\\)$" :middle "|\s?" :end "`---+$"))
  "Alist of block regexp."
  :group 'comment-edit
  :type 'alist)

(defvar comment-edit--line-delimiter nil "Comment delimiter of each editing line.")

(defvar comment-edit-debug-p nil)

(defvar comment-edit-leave-blank-line-in-comment nil
  "Leave blank lines in comment without left-padding.")

;;; Utils

(defun comment-edit-toggle-debug (&optional debug-p)
  (interactive)
  (setq comment-edit-debug-p (or debug-p (not comment-edit-debug-p)))
  (message "comment-edit-debug-p => %s" comment-edit-debug-p))

(defun comment-edit--log (format-string &rest args)
  (when comment-edit-debug-p
    (if noninteractive
        (funcall 'message format-string args)
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
  (cl-rassoc item
             list
             :test
             (lambda (item it)
               (if (listp it)
                   (memq item it)
                 (eq it item)))))

(defun comment-edit--get-real-mode (&option mode)
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
  "Return region of string at point POS"
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
  "Return the face if point at comment."
  (let ((face (get-text-property (or point (point)) 'face)))
    (or (memq face '(font-lock-comment-face font-lock-comment-delimiter-face))
        (when (apply #'derived-mode-p '(c-mode c++-mode java-mode js-mode rust-mode))
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
        '-----------------------+
"
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
                        end -`
"
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
  "Return the region of continuous comments.

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
                   end -`
"
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
  (concat (caar (comment-edit--get-comment-encloser (or mode major-mode)))
          "[\t\s]*"
          (when multi-line-p
            "\n")))

(defun comment-edit--comment-end-encloser (&optional multi-line-p mode)
  (concat (when multi-line-p
            "\n")
          "[\t\s]*"
          (cadar (comment-edit--get-comment-encloser (or mode major-mode)))))

(defun comment-edit--get-comment-encloser (&optional mode)
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
  "Return code block info contains :beginning."
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
                 (list
                  :beginning (point-at-bol)
                  :lang-mode
                  (or (comment-edit-get-lang-mode (or (match-string-no-properties 1) ""))
                      major-mode)
                  :regexps
                  (car
                   (-non-nil
                    (--map (when (string-match-p
                                  (plist-get it :beginning)
                                  (match-string-no-properties 0))
                             it)
                           comment-edit-block-regexp-plist))))))))))

(defun comment-edit--code-block-end (code-info &optional comment-delimiter)
  "Return CODE-INFO with :end added."
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
  (let ((aval (assoc-default mode comment-edit-string-quotes-alist)))
    (cond ((symbolp aval) (assoc-default (or aval t) comment-edit-string-quotes-alist))
          (t aval))))

(defun comment-edit--block-info ()
  "Return block info at point.

Block info example:

    (:beginning 10
     :lang-mode emacs-lisp-mode
     :regexps (:beginning \"``` ?\\(\\w*\\)$\" :middle nil :end \"```$\")
     :end 12
     :in-str-p nil)

:regexps        not nil means point at a code block.
:in-str-p       not nil means point at a string block otherwish a comment block."
  (let ((strp (comment-edit--point-at-string)))
    (save-restriction
      (apply 'narrow-to-region
             (if strp
                 (let ((region (comment-edit--string-region)))
                   (setq strp (comment-edit--string-quotes
                               (comment-edit--string-beginning)))
                   region)
               (comment-edit--comment-region)))
      (let* ((delimiter (unless strp (comment-edit--comment-delimiter-regexp)))
             (code-info (comment-edit--code-block-end
                         (comment-edit--code-block-beginning delimiter)
                         delimiter)))
        (if (and (plist-get code-info :beginning) (plist-get code-info :end))
            (plist-put code-info :in-str-p strp)
          (plist-put
           (plist-put
            (plist-put code-info :beginning (point-min))
            :end (point-max))
           :in-str-p strp))))))

;;; comment-edit-mode

(defun comment-edit--remove-comment-delimiter (regexp)
  "Remove comment delimiter of each line by REGEXP when entering comment-edit-mode."
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
             comment-edit--line-delimiter)
    (let ((delimiter (if (string-suffix-p " " comment-edit--line-delimiter)
                         comment-edit--line-delimiter
                       (concat comment-edit--line-delimiter " "))))
      (save-excursion
        (goto-char beg)
        (catch 'end-of-buffer
          (while (re-search-forward "^.*$" nil t)
            (let* ((str (string-trim-right (match-string 0)))
                   (leave-blank-p (and comment-edit-leave-blank-line-in-comment
                                      (string-empty-p str))))
              (replace-match (if leave-blank-p "" (concat delimiter str)))
              (when leave-blank-p
                (unless (zerop (forward-line 1))
                  (throw 'end-of-buffer nil))))))))))

(defun comment-edit--remove-nested-escape ()
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (1- (math-pow 2 num))))
        (when (and (< 0 match-len)
                   (looking-back (eval `(rx (not (any "\\")) (= ,match-len "\\"))) 1))
          (let ((del-len (- match-len (1- (math-pow 2 (1- num))))))
            (backward-delete-char del-len)
            (throw 'break nil)))))))

(defun comment-edit--remove-nested-escape-sq ()
  (catch 'break
    (dolist (num (number-sequence 1 9))
      (let ((match-len (- (math-pow 2 num) 2)))
        (when (and (< 0 match-len)
                   (looking-back (eval `(rx (not (any "\\")) (= ,match-len "\\"))) 1))
          (let ((del-len (- match-len (1- (math-pow 2 (1- num))))))
            (backward-delete-char del-len)
            (throw 'break nil)))))))

(defun comment-edit--remove-escape (quotes-char)
  "Remove escape when editing docstring."
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
  "Restore escape when finished edting docstring."
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

(defvar comment-edit-mode-map (make-sparse-keymap) "Keymap for `comment-edit-mode'.")

(define-minor-mode comment-edit-mode
  "Minor mode for enable edit code block in comment.\\{comment-edit-mode-map}"
  :init-value nil
  :group 'comment-edit
  :global nil
  :keymap 'comment-edit-mode-map
  )

(defvar comment-edit-double-quote-string-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'comment-edit)
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
    (define-key map (kbd "C-c C-e") #'comment-edit)
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
  "Edit comment or docstring or code block in them."
  (interactive)
  (let* ((block (or block (comment-edit--block-info)))
         (beg (plist-get block :beginning))
         (end (plist-get block :end))
         (lang-mode (plist-get block :lang-mode))
         (strp (plist-get block :in-str-p))
         (commentp (not strp))
         (codep (and (plist-get block :regexps) t))
         (delimiter-regexp (concat (if strp "^\s*"
                                     (comment-edit--comment-delimiter-regexp))
                                   (plist-get (plist-get block :regexps) :middle))))
    (comment-edit--log "==> block-info: %S" block)
    ;; (comment-edit--log "==> block: %S" (buffer-substring-no-properties beg end))
    (if block
        (let* ((mode
                (if codep
                    (or lang-mode
                        comment-edit-code-block-default-mode)
                  (cond ((and (stringp strp) (string= "'" strp)) 'comment-edit-single-quote-string-mode)
                        ((and (stringp strp) (string= "\"" strp)) 'comment-edit-double-quote-string-mode)
                        (t comment-edit-default-mode)))))
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
                           (set (make-local-variable 'header-line-format)
                                (substitute-command-keys (concat "*EDIT* "
                                                                 (mapconcat
                                                                  'identity
                                                                  (-non-nil
                                                                   (list "\\[edit-indirect-commit]: Exit"
                                                                         "\\[edit-indirect-abort]: Abort"
                                                                         (when ,strp
                                                                           "\\[comment-edit]: Recursive-entry")))
                                                                  ", "))))
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
