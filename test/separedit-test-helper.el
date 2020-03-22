;;; separedit-test-helper.el --- Helpers of testing -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(defvar test-expr nil
  "Holds a test expression to evaluate with `test-eval'.")

(defvar test-result nil
  "Holds the eval result of `test-expr' by `test-eval'.")

(defun test-eval ()
  "Evaluate `test-expr'."
  (interactive)
  (setq test-result (eval test-expr)))

(global-set-key (kbd "C-c C-c e") 'test-eval)

(defun test-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((test-expr expr))
    (execute-kbd-macro
     (vconcat (kbd (if expr "C-c C-c e" ""))
              (kbd keys)))
    test-result))

;;; polyfil

(unless (fboundp 'indent-region-line-by-line)
  (defun indent-region-line-by-line (beg end)
    (save-excursion
      (goto-char beg)
      (catch 'break
        (while t
          (sh-indent-line)
          (when (< 0 (forward-line 1))
            (throw 'break nil)))))))

(unless (fboundp 'font-lock-ensure)
  (defun font-lock-ensure (&optional beg end)
    (font-lock-set-defaults)
    (funcall 'jit-lock-fontify-now
             (or beg (point-min)) (or end (point-max)))))

;;;

(defun separedit-test--region-between-regexps (begin-regexp end-regexp)
  "Return region between BEGIN-REGEXP and END-REGEXP."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward begin-regexp)
      (let ((begin (point)))
        (when (re-search-forward end-regexp nil t)
          (goto-char (match-beginning 0))
          (list :beginning begin :end (point)))))))

(defun --join\n (&rest strings)
  (mapconcat #'identity
             strings
             "\n"))

(defun --bufs= (string)
  "Verify whether buffer string equals STRING."
  (should (string= string
                   (buffer-substring-no-properties (point-min)
                                                   (point-max)))))

(defun --mode= (mode)
  "Verify whether major mode equals MODE."
  (should (eq mode major-mode)))

(defun --key= (&rest args)
  "Verify keybindings.

ARGS is a list in the form of `KEY-STR CMD-SYM ...'."
  (->> (-partition 2 args)
       (--map (string= (car it) (substitute-command-keys (format "\\[%s]" (cadr it)))))
       (-none? 'null)))

(defun --with-callback (init-mode init-data key-sequnce callback &optional region-regexps)
  "Execute CALLBACK after the KEY-SEQUNCE pressed in edit buffer.

INIT-MODE       major-mode of source buffer
INIT-DATA       initial data of source buffer
REGION-REGEXPS  regexp for detection block in source buffer"
  (let ((buf (generate-new-buffer "*init*")))
    (switch-to-buffer buf)
    (insert init-data)
    (funcall init-mode)
    ;; Force enable face / text property / syntax highlighting
    (let ((noninteractive nil))
      (font-lock-mode 1)
      (font-lock-ensure))
    (goto-char (point-min))
    (re-search-forward "<|>")
    (separedit (when region-regexps
                 (apply #'separedit-test--region-between-regexps region-regexps)))
    (test-with nil key-sequnce)
    (funcall callback)))

(defun separedit-test--execute-block-edit (init-mode key-sequnce init-data expected-data &optional region-regexps)
  (let ((buf (generate-new-buffer "*init*")))
    (switch-to-buffer buf)
    (insert init-data)
    (funcall init-mode)
    ;; Force enable face / text property / syntax highlighting
    (let ((noninteractive nil))
      (font-lock-mode 1)
      (font-lock-ensure))
    (goto-char (point-min))
    (re-search-forward "<|>")
    (separedit (when region-regexps
                    (apply #'separedit-test--region-between-regexps region-regexps)))
    (test-with nil key-sequnce)
    (should
     (equal expected-data
            (buffer-substring-no-properties (point-min) (point-max))))))

(defun separedit-test--append-to-code-block (mode string append)
  "Insert APPEND into the tail of code block in comment of STRING.

Example:

    Input:

    ;; ```elisp
    ;; (1+ 1)
    ;; ```

    Output:

    ;; ```elisp
    ;; (1+ 1)INSERT-APPEND-HERE
    ;; ```"
  (with-current-buffer (generate-new-buffer "*append*")
    (insert string)
    (funcall mode)
    ;; Force enable face / text property / syntax highlighting
    (let ((noninteractive nil))
      (font-lock-mode 1)
      (font-lock-ensure))
    (goto-char (point-min))
    (re-search-forward "<|>")
    (let ((block (separedit--block-info)))
      (goto-char (plist-get block :end))
      (insert append)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun separedit-test--indent (mode string &optional indent-fn)
  (with-current-buffer (generate-new-buffer "*indent*")
    (insert string)
    (funcall mode)
    (funcall (or indent-fn 'indent-region) (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun separedit-test--indent-c (&rest strings)
  (separedit-test--indent 'c-mode (apply #'concat strings)))

(defun separedit-test--indent-sh (string)
  (separedit-test--indent 'shell-script-mode string 'indent-region-line-by-line))

(defun separedit-test--indent-el (string)
  (separedit-test--indent 'emacs-lisp-mode string))

(defun separedit-test--indent-py (string)
  (separedit-test--indent 'python-mode string))

(defun separedit-test--indent-rb (string)
  (separedit-test--indent 'ruby-mode string))

(defun separedit-test--indent-pascal (string)
  (separedit-test--indent 'pascal-mode string))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-07-04"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef[:digit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(defmacro separedit-test--with-buffer (mode content &rest body)
  (declare (indent 1) (debug t))
  `(let ((buf (generate-new-buffer "*separedit-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,content)
           (funcall ,mode)
           (goto-char (point-min))
           (re-search-forward "<|>" nil t 1)
           (let ((noninteractive nil))
             (font-lock-mode 1)
             (font-lock-set-defaults)
             (jit-lock-fontify-now (point-min) (point-max)))
           ,@body)
       (kill-buffer buf))))

(defun escape (&rest str-list)
  "Nest escaping quoted strings in STR-LIST.
\(fn s1 s2 s3 ...)
=> (make-escape
    (concat
     s1
     (make-escape
      (concat
       s2
       (make-escape
        (concat
         s3
         ...))))))"
  (let ((s ""))
    (mapc (lambda (it)
            (setq s (format "%S" (concat it s))))
          (reverse str-list))
    s))

;; Make sure `escape' correct before testing.
(require 'cl-macs)
(cl-assert (string= (escape "a" "b" "c" "d" "e") (format "%S" "a\"b\\\"c\\\\\\\"d\\\\\\\\\\\\\\\"e\\\\\\\\\\\\\\\"\\\\\\\"\\\"\"")))
(cl-assert (string= (escape "b" "c" "d" "e")     (format "%S" "b\"c\\\"d\\\\\\\"e\\\\\\\"\\\"\"")))
(cl-assert (string= (escape "c" "d" "e")         (format "%S" "c\"d\\\"e\\\"\"")))
(cl-assert (string= (escape "d" "e")             (format "%S" "d\"e\"")))
(cl-assert (string= (escape "e")                 (format "%S" "e")))

(defun escape-sq (&rest str-list)
  "Nest escaping single-quoted strings in STR-LIST.
\(fn s1 s2 s3 ...)
=> (make-escape
    (concat
     s1
     (make-escape
      (concat
       s2
       (make-escape
        (concat
         s3
         ...))))))"
  (replace-regexp-in-string "\"" "'" (apply #'escape str-list)))

(defun nest-and-assert (curr &rest nexts)
  ;;; remove escape
  (save-restriction
    (goto-char (point-min))
    (search-forward "\"")
    (apply 'narrow-to-region (separedit--string-region))
    (separedit--remove-escape "\"")
    (should (string= (car curr) (format "%S" (buffer-substring-no-properties (point-min) (point-max)))))
    (when nexts
      (apply #'nest-and-assert nexts))
  ;;; restore escape
    (separedit--restore-escape "\""))
  (should (string= (cdr curr) (format "%S" (buffer-substring-no-properties (point-min) (point-max))))))

(defun nest-and-assert-sq (curr &rest nexts)
  ;;; remove escape
  (save-restriction
    (goto-char (point-min))
    (search-forward "'")
    (apply 'narrow-to-region (separedit--string-region))
    (separedit--remove-escape "'")
    (should (string= (car curr)
                     (replace-regexp-in-string
                      "\"" "'"
                      (format "%S" (replace-regexp-in-string ;; Convert to double-quoted string, then \
                                    "'" "\""                 ;; use `format' to add escape characters.
                                    (buffer-substring-no-properties (point-min) (point-max)))))))
    (when nexts
      (apply #'nest-and-assert-sq nexts))
  ;;; restore escape
    (separedit--restore-escape "'"))
  (should (string= (cdr curr)
                     (replace-regexp-in-string
                      "\"" "'"
                      (format "%S" (replace-regexp-in-string ;; Convert to double-quoted string, then \
                                    "'" "\""                 ;; use `format' to add escape characters.
                                    (buffer-substring-no-properties (point-min) (point-max))))))))

(defun separedit-test--generate-readme ()
  (with-temp-buffer
    (insert-file-contents "separedit.el")
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let* ((reg (or (separedit-test--region-between-regexps "^;;; Commentary:\n+" "\n;;; .*$")
                    (error "Commentary not found in current file!")))
           (str (buffer-substring-no-properties (plist-get reg :beginning) (plist-get reg :end))))
      (with-temp-buffer
        (insert str)
        (separedit--remove-comment-delimiter
         (separedit--comment-delimiter-regexp 'emacs-lisp-mode))
        (buffer-string)))))

;;; separedit-test-helper.el ends here
