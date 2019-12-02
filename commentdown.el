;;; commentdown.el --- Code block in comment or docstring -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/06
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "2.0") (edit-indirect "0.1.5"))
;; URL: https://github.com/twlz0ne/commentdown.el
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

;; ## Configuration:
;;
;; ```
;; (require 'commentdown)
;; (define-key prog-mode-map (kbd "C-c '") #'commentdown-edit)
;; ```

;;; Change Log:

;;  0.1.0  2019/04/06  Initial version.

;;; Code:

(require 'dash)
(require 'edit-indirect)

(defcustom commentdown-edit-code-block-default-mode 'normal-mode
  "Default mode to use for editing code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'commentdown
  :type '(choice function (const :tag "None" nil)))

(defcustom commentdown-code-lang-modes
'(("C"         . c-mode)                ("C++"   . c++-mode)
  ("asymptote" . asy-mode)              ("bash"  . sh-mode)
  ("calc"      . fundamental-mode)      ("cpp"   . c++-mode)
  ("ditaa"     . artist-mode)           ("dot"   . fundamental-mode)
  ("elisp"     . emacs-lisp-mode)       ("ocaml" . tuareg-mode)
  ("screen"    . shell-script-mode)     ("shell" . sh-mode)
  ("sqlite"    . sql-mode))
  "Alist mapping languages to their major mode.
Taken from `markdown-code-lang-modes'."
  :group 'commentdown
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode"))))

(defcustom commentdown-comment-regexp-alist
  '((emacs-lisp-mode    . (";+"))
    (c-mode             . ("//+" "\\*+"))
    (c++-mode           . c-mode)
    (python-mode        . ("#+"))
    (ruby-mode          . ("#+")))
  "Alist of comment regexp."
  :group 'commentdown
  :type 'alist)

(defcustom commentdown-block-regexp-plist
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
  :group 'commentdown
  :type 'alist)

(defvar commentdown--line-starter nil "Line starter of each editing line.")

(defvar commentdown-debug-p nil)

;;; Utils

(defun commentdown-toggle-debug (&optional debug-p)
  (interactive)
  (setq commentdown-debug-p (or debug-p (not commentdown-debug-p)))
  (commentdown--log "commentdown-debug-p => %s" commentdown-debug-p))

(defun commentdown--log (format-string &rest args)
  (when commentdown-debug-p
     (if noninteractive
         (funcall 'message format-string args)
       (with-current-buffer (get-buffer-create "*comment-log*")
         (outline-mode)
         (buffer-disable-undo)
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert (apply 'format (cons format-string args))
                   "\n"))))))

(defun commentdown--end-of-previous-line (&optional pos)
  "Move cursor to the end of prevous line of the given point POS.

Return nil if reached the beginning of the buffer."
  (when pos
    (goto-char pos))
  (condition-case err
      (forward-line -1)
    (error nil))
  (end-of-line)
  t)

(defun commentdown--beginning-of-next-line (&optional pos)
  "Move cursor to the beginning of next line of the given point POS.

Return nil if reached the end of the buffer."
  (when pos
    (goto-char pos))
  (condition-case err
      (forward-line 1)
    (error nil))
  (beginning-of-line)
  t)

;;; Docstring funcitons

(defun commentdown--point-at-string (&optional pos)
  "Determine if point POS at string or not."
  (let* ((prop-value (get-text-property (or pos (point)) 'face))
         (testfn (if (listp prop-value) 'memq 'eq)))
    (and (or (funcall testfn 'font-lock-string-face prop-value)
             (funcall testfn 'font-lock-doc-face prop-value)
             (funcall testfn 'font-lock-constant-face prop-value))
         t)))

(defun commentdown--string-beginning (&optional pos)
  "Return beginning of string at point POS"
  (let ((pos (or pos (point)))
        (new-pos))
    (when (commentdown--point-at-string pos)
      (catch 'break
        (while t
          (setq new-pos (previous-single-property-change pos 'face))
          (cond ((not new-pos) (throw 'break (point-max)))
                ((not (commentdown--point-at-string new-pos)) (throw 'break pos))
                (t (setq pos new-pos))))))))

(defun commentdown--string-end (&optional pos)
  "Return end of string at point POS"
  (let ((pos (or pos (point)))
        (new-pos))
    (when (commentdown--point-at-string pos)
      (catch 'break
        (while t
          (setq new-pos (next-single-property-change pos 'face))
          (cond ((not new-pos) (throw 'break (point-max)))
                ((not (commentdown--point-at-string (1- new-pos))) (throw 'break pos))
                (t (setq pos new-pos))))))))

(defun commentdown--string-region (&optional pos)
  "Return region of string at point POS"
  (let ((pos (or pos (point))))
    (if (commentdown--point-at-string pos)
        (let ((fbeg (commentdown--string-beginning pos))
              (fend (commentdown--string-end       pos)))
          (list (1+ (or fbeg (point-min)))
                (1- (or fend (point-max)))))
      (user-error "Not inside a string"))))

;;; Comment functions

(defun commentdown--comment-starter-regexp (&optional mode)
  "Return comment starter regex of MODE."
  (let* ((mode (or mode major-mode))
         (def (or (assoc mode commentdown-comment-regexp-alist)
                  (assoc (get mode 'derived-mode-parent) commentdown-comment-regexp-alist))))
    (if (symbolp (cdr def))
        (commentdown--comment-starter-regexp (cdr def))
      (concat "^\s*\\(?:"
              (mapconcat 'identity (cdr def) "\\|")
              "\\)\s?"))))

(defun commentdown--point-at-comment (&optional pos)
  "Determine if point POS at comment, or at the leading blank front of comment.

style 1:

         ,----------- face: nil
         |    .------ face: font-lock-comment-delimiter-face
         |   |   .--- face: font-lock-comment-face
         |   |   |
        +|---|---|----------.
        |   ;; comment      |
        `-------------------+

style 2:
          ,------- face: nil
          |  ,---- face: font-lock-comment-delimiter-face
          |  |  ,- face: font-lock-comment-face
        +-|--|--|-----------,
        |   /* comment      |
        |    * comment      |
        `-|--|--|-----------+
          |  |  `- face: font-lock-comment-face
          |  `---- face: font-lock-comment-face
          `------- face: font-lock-comment-face"
  (let* ((pos (min (or pos (point)) (1- (point-max)))) ;; there is no text properties at point-max
         (prop-value (get-text-property pos 'face))
         (testfn (if (listp prop-value) 'memq 'eq)))
    (commentdown--log
     "==> [commentdown--point-at-comment] %S"
     (list :point pos
           :char (char-to-string (char-before pos))
           :props (text-properties-at pos)))
    (if (or (funcall testfn 'font-lock-comment-delimiter-face prop-value)
            (funcall testfn 'font-lock-comment-face prop-value))
        t
      (let ((eol (save-excursion
               (goto-char pos)
               (end-of-line)
               (point))))
        (unless (or (eq pos eol) (eq pos (1- (point-max))))
          (commentdown--point-at-comment eol))))))

(defun commentdown--comment-beginning (&optional pos)
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
  (let ((pos (or pos (point)))
        (new-pos))
    (when (commentdown--point-at-comment pos)
      (save-excursion
        (setq new-pos
              (catch 'break
                (while t
                  (forward-line -1)
                  (end-of-line)
                  (setq new-pos (point))
                  (cond ((eq pos new-pos) (throw 'break new-pos))
                        ((commentdown--point-at-comment new-pos) (setq pos new-pos))
                        (t (throw 'break pos))))))
        (when new-pos
          (goto-char new-pos)
          (beginning-of-line)
          (point))))))

(defun commentdown--comment-end (&optional pos)
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
  (let ((pos (or pos (point)))
        (new-pos))
    (when (commentdown--point-at-comment pos)
      (save-excursion
        (setq new-pos
              (catch 'break
                (while t
                  (forward-line 1)
                  (end-of-line)
                  (setq new-pos (point))
                  (cond ((eq pos new-pos) (throw 'break new-pos))
                        ((commentdown--point-at-comment new-pos) (setq pos new-pos))
                        (t (throw 'break pos))))))
        (when new-pos
          (goto-char new-pos)
          (end-of-line)
          (point))))))

(defun commentdown--comment-region (&optional pos)
  "Return the region of continuous comments.

           .- beginning
          /
        +|----------------------.
        ||      ;; comment      |
        |       ;; comment      |
        |       ;; comment     ||
        '----------------------|+
                              /
                        end -`
"
  (let ((pos (or pos (point))))
    (commentdown--log "==> [commentdown--comment-region] pos: %s" pos)
    ;; (commentdown--log "==> [commentdown--comment-region] buffer string: %S"
    ;;          (buffer-substring-no-properties (point-min) (point-max)))
    (if (commentdown--point-at-comment pos)
        (let ((fbeg (commentdown--comment-beginning pos))
              (fend (commentdown--comment-end       pos)))
          (list (save-excursion
                  ;; Skip `/*' for C/C++
                  (goto-char (or fbeg (point-min)))
                  (re-search-forward (commentdown--comment-starter-regexp))
                  (match-beginning 0))
                (save-excursion
                  (goto-char (or fend (point-max)))
                  ;; Skip `*/' for C/C++
                  (re-search-backward (commentdown--comment-starter-regexp))
                  (point-at-eol))))
      (user-error "Not inside a comment"))))

;;; Code block functions

(defun commentdown--code-block-beginning (&optional comment-starter)
  "Return code block info contains :beginning."
  (let ((regexp-group
         (concat
          comment-starter
          "\\(?:"
          (mapconcat
           (lambda (it)
             (plist-get it :beginning))
           commentdown-block-regexp-plist
           "\\|")
          "\\)")))
    (catch 'break
      (save-excursion
        (commentdown--log "==> [code-block-beginning] comment-starter: %S" comment-starter)
        (commentdown--log "==> [code-block-beginning] regexp-group: %S" regexp-group)
        (when (re-search-backward regexp-group nil t)
          (save-match-data
            (commentdown--log "==> [code-block-beginning] matched1: %s" (match-string-no-properties 1))
            (commentdown--log "==> [code-block-beginning] language: %s" (car (rassoc major-mode commentdown-code-lang-modes))))
          (commentdown--beginning-of-next-line)
          (throw 'break
                 (list
                  :beginning (point-at-bol)
                  :lang
                  (or (let ((lang (match-string-no-properties 1)))
                        (unless (string= "" lang)
                          lang))
                      (car (rassoc major-mode commentdown-code-lang-modes)))
                  :regexps
                  (car
                   (-non-nil
                    (--map (when (string-match-p
                                  (plist-get it :beginning)
                                  (match-string-no-properties 0))
                             it)
                           commentdown-block-regexp-plist))))))))))

(defun commentdown--code-block-end (code-info &optional comment-starter)
  "Return CODE-INFO with :end added."
  (save-excursion
    (let ((regexp (concat comment-starter
                          (plist-get
                           (plist-get code-info :regexps)
                           :end))))
      (when (re-search-forward regexp nil t)
        (commentdown--end-of-previous-line)
        (plist-put code-info
                   :end (point-at-eol))))))

(defun commentdown-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   'fboundp
   (list (cdr (assoc lang commentdown-code-lang-modes))
         (cdr (assoc (downcase lang) commentdown-code-lang-modes))
         (intern (concat lang "-mode"))
         (intern (concat (downcase lang) "-mode")))))

(defun commentdown--block-info ()
  "Return block info at point.

Block info example:

    (:beginning 10
     :lang \"elisp\"
     :regexps (:beginning \"``` ?\\(\\w*\\)$\" :middle nil :end \"```$\")
     :end 12
     :in-str-p nil)

:regexps        not nil means point at a code block.
:in-str-p       is t means point at a string block otherwish a comment block."
  (let ((strp (commentdown--point-at-string)))
    (save-restriction
        (apply 'narrow-to-region
               (if strp
                   (commentdown--string-region)
                 (commentdown--comment-region)))
      (let* ((starter (unless strp (commentdown--comment-starter-regexp)))
             (code-info (commentdown--code-block-end
                         (commentdown--code-block-beginning starter)
                         starter)))
        (if (and (plist-get code-info :beginning) (plist-get code-info :end))
            (plist-put code-info :in-str-p strp)
          (plist-put
           (plist-put
            (plist-put code-info :beginning (point-min))
            :end (point-max))
           :in-str-p strp))))))

;;; commentdown-mode

(defun commentdown--remove-comment-starter (regexp)
  "Remove comment starter of each line by REGEXP when entering commentdown-mode."
  (let ((line-starter)
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (catch 'break
        (while (and (< (point-min) (point)) (re-search-backward regexp nil t))
          (unless line-starter
            (setq line-starter (match-string 0)))
          (replace-match "")
          (when (eq (point) (point-min))
            (throw 'break nil))
          (backward-char))))
    line-starter))

(defun commentdown--restore-comment-starter (beg end)
  "Restore comment starter of each line between BEG to END when returning from comemntdown-mode."
  (commentdown--log "==> [commentdown--restore-comment-starter] line starter: %s"
              commentdown--line-starter)
  (when (and (string-prefix-p "*edit-indirect " (buffer-name))
             commentdown--line-starter)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^.*$" nil t)
        (replace-match (concat commentdown--line-starter
                               (match-string 0)))))))

(defun commentdown--remove-escape ()
  "Remove escape when editing docstring."
  (goto-char (point-max))
  (while (re-search-backward "\\\\\"" nil t)
    (replace-match "\"")))

(defun commentdown--restore-escape ()
  "Restore escape when finished edting docstring."
  (goto-char (point-min))
  (while (re-search-forward "\"" nil t)
    (replace-match "\\\\\"")))

(defvar commentdown-mode-map (make-sparse-keymap) "Keymap for `commentdown-mode'.")

(define-minor-mode commentdown-mode
  "Minor mode for enable edit code block in comment.\\{commentdown-mode-map}"
  :init-value nil
  :group 'commentdown
  :global nil
  :keymap 'commentdown-mode-map
  )

;;;###autoload
(defun commentdown-edit ()
  "Edit comment or docstring or code block in them."
  (interactive)
  (let* ((block (commentdown--block-info))
         (beg (plist-get block :beginning))
         (end (plist-get block :end))
         (lang (plist-get block :lang))
         (strp (plist-get block :in-str-p))
         (commentp (not strp))
         (codep (and (plist-get block :regexps) t))
         (starter-regexp (concat (if strp "^\s*"
                                     (commentdown--comment-starter-regexp))
                                 (plist-get (plist-get block :regexps) :middle))))
    (commentdown--log "==> block-info: %S" block)
    ;; (commentdown--log "==> block: %S" (buffer-substring-no-properties beg end))
    (if block
        (let* ((mode (if codep (and lang (commentdown-get-lang-mode lang))
                         commentdown-edit-code-block-default-mode)))
          (setq-local edit-indirect-guess-mode-function
                      `(lambda (_parent-buffer _beg _end)
                         (let ((line-starter (and (or ,codep ,commentp) (commentdown--remove-comment-starter ,starter-regexp))))
                           (when ,strp
                             (commentdown--remove-escape))
                           (funcall ',mode)
                           ;; (commentdown--log "==> block(edit buffer): %S"
                           ;;                   (buffer-substring-no-properties (point-min) (point-max)))
                           (set (make-local-variable 'commentdown--line-starter) line-starter)
                           (set (make-local-variable 'edit-indirect-before-commit-hook)
                                (append '((lambda ()
                                            (commentdown--restore-comment-starter (point-min) (point-max))
                                            (when ,strp
                                              (commentdown--restore-escape))))
                                        edit-indirect-before-commit-hook)))))
          (edit-indirect-region beg end 'display-buffer))
      (user-error "Not inside a code block"))))

(provide 'commentdown)

;;; commentdown.el ends here
