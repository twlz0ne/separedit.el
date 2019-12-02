;;; test-helper.el --- Helpers of testing -*- lexical-binding: t; -*-

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

(defun test-commentdown--execute-block-edit (init-mode key-sequnce init-data expected-data)
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
    (commentdown-edit)
    (test-with nil key-sequnce)
    (should
     (equal expected-data
            (buffer-substring-no-properties (point-min) (point-max))))))

(defun test-commentdown--append-to-code-block (mode string append)
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
    (let ((block (commentdown--block-info)))
      (goto-char (plist-get block :end))
      (insert append)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun test-commentdown--indent (mode string &optional indent-fn)
  (with-current-buffer (generate-new-buffer "*indent*")
    (insert string)
    (funcall mode)
    (funcall (or indent-fn 'indent-region) (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun test-commentdown--indent-c (string)
  (test-commentdown--indent 'c-mode string))

(defun test-commentdown--indent-sh (string)
  (test-commentdown--indent 'shell-script-mode string 'indent-region-line-by-line))

(defun test-commentdown--indent-el (string)
  (test-commentdown--indent 'emacs-lisp-mode string))

(defun test-commentdown--indent-py (string)
  (test-commentdown--indent 'python-mode string))

(defun test-commentdown--indent-rb (string)
  (test-commentdown--indent 'ruby-mode string))

;;; test-helper.el ends here
