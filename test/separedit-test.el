;;; separedit-test.el --- Test separedit -*- lexical-binding: t; -*-

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

(require 'cl-macs)

(require 'ert)
;; (setq ert-batch-backtrace-right-margin nil)

(require 'separedit)
;; (separedit-toggle-debug t)

(when noninteractive
  (transient-mark-mode))

;;; Function test

(ert-deftest separedit-test-beginning-of-comment-el ()
  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3")
   (should (eq (separedit--comment-beginning) (+ (point-min) 0))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "
     ;; comment1
     ;; comment2<|>
     ;; comment3")
   (should (eq (separedit--comment-beginning) (+ (point-min) 1))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "\n
     ;; comment1
     ;; comment2<|>
     ;; comment3\n\n")
   (should (eq (separedit--comment-beginning) (+ (point-min) 2))))

  ;;;
  
  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3")
   (should (eq (separedit--comment-beginning) (+ (point-min) 0))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "
           ;; comment1
           ;; comment2<|>
           ;; comment3\n")
   (should (eq (separedit--comment-beginning) (+ (point-min) 1))))
  
  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "\n
         ;; comment1
          ;; comment2<|>
          ;; comment3\n\n")
   (should (eq (separedit--comment-beginning) (+ (point-min) 2)))))

(ert-deftest separedit-test-end-of-comment-el ()
  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3")
   (should (eq (separedit--comment-end) (- (point-max) 0))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3\n")
   (should (eq (separedit--comment-end) (- (point-max) 1))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3\n\n")
   (should (eq (separedit--comment-end) (- (point-max) 2))))

  ;;;

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3")
   (should (eq (separedit--comment-end) (- (point-max) 0))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3\n")
   (should (eq (separedit--comment-end) (- (point-max) 1))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3\n\n")
   (should (eq (separedit--comment-end) (- (point-max) 2))))

  ;;; With trailing empty comment line

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3
     ;;")
   (should (eq (separedit--comment-end) (- (point-max) 0))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3
     ;; ")
   (should (eq (separedit--comment-end) (- (point-max) 0))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3
     ;;\n")
   (should (eq (separedit--comment-end) (- (point-max) 1)))))

(ert-deftest separedit-test-region-of-comment-c1 ()
  ;;; Without leading spaces
  
  ;; Without blank lines

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (point-max)))))

  ;; Blank lines at the end
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3\n")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (- (point-max) 1)))))
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3\n\n")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (- (point-max) 2)))))

  ;; Blank lines at the bebinning

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n"
    "// comment1
     // comment2<|>
     // comment3")
   (should
    (equal
     (separedit--comment-region)
     (list (+ (point-min) 1)
           (point-max)))))
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n\n"
    "// comment1
     // comment2<|>
     // comment3")
   (should
    (equal
     (separedit--comment-region)
     (list (+ (point-min) 2)
           (point-max)))))

  ;;; With leading spaces

  ;; Without blank lines

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "   // comment1
        // comment2<|>
        // comment3")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (point-max)))))

  ;; Blank lines at the end

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "   // comment1
        // comment2<|>
        // comment3\n")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (- (point-max) 1)))))

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "   // comment1
        // comment2<|>
        // comment3\n\n")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (- (point-max) 2)))))

  ;; Blank lines at the end

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n"
    "   // comment1
        // comment2<|>
        // comment3")
   (should
    (equal
     (separedit--comment-region)
     (list (+ (point-min) 1)
           (point-max)))))

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n\n"
    "   // comment1
        // comment2<|>
        // comment3")
   (should
    (equal
     (separedit--comment-region)
     (list (+ (point-min) 2)
           (point-max)))))

  ;;; With trailing empty comment line

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3
     //")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (point-max)))))

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3
     // ")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (point-max)))))

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3
     //\n")
   (should
    (equal
     (separedit--comment-region)
     (list (point-min)
           (- (point-max) 1))))))

(ert-deftest separedit-test-region-of-comment-c2 ()
  ;;; Without leading spaces

  ;; Without blank lines
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "/*
      * comment1
      * comment2<|>
      * comment3
      */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the end
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "/*
      * comment1
      * comment2<|>
      * comment3
      */\n")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "/*
      * comment1
      * comment2<|>
      * comment3
      */\n\n")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the beginning
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n"
    "/*
      * comment1
      * comment2<|>
      * comment3
      */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))
  
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n\n"
    "/*
      * comment1
      * comment2<|>
      * comment3
      */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;;; With leading spaces

  ;; Without blank lines

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the end

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */\n")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */\n\n")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the beginning

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n"
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "\n\n"
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t)))))))

(ert-deftest separedit-test-region-of-comment-c3 ()
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "/*
        comment1
        comment2<|>
        comment3
      */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-* comment3$" nil t)))))))

(ert-deftest separedit-test-region-of-comment-pascal ()
  (separedit-test--with-buffer
   'pascal-mode
   (separedit-test--indent-pascal
    "{
     comment1
     comment2<|>
     comment3
     }")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*comment3$" nil t)))))
   ))

(ert-deftest separedit-test-region-of-single-line-comment ()
  (separedit-test--with-buffer
   'c-mode
   "/* com<|>ment */"
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (goto-char (point-min)) (search-forward "/* "))
           (save-excursion (goto-char (point-max)) (search-backward " */"))))))
  (separedit-test--with-buffer
   'c-mode
   "/** com<|>ment */"
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (goto-char (point-min)) (search-forward "/** "))
           (save-excursion (goto-char (point-max)) (search-backward " */"))))))
  (separedit-test--with-buffer
   'pascal-mode
   "{ com<|>ment }"
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (goto-char (point-min)) (search-forward "{ "))
           (save-excursion (goto-char (point-max)) (search-backward " }")))))))

(ert-deftest separedit-test-comment-at-end-of-comment ()
  (separedit-test--with-buffer
   'emacs-lisp-mode
   ";; comment-without-trailing-spaces"
   (should (not (separedit--point-at-comment (point-max)))))

  (separedit-test--with-buffer
   'emacs-lisp-mode
   ";; comment-with-trailing-spaces "
   (should (not (separedit--point-at-comment (point-max))))))

(ert-deftest separedit-test-comment-delimiter-regexp-el ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (separedit--comment-delimiter-regexp 'emacs-lisp-mode) "" (car it)))))
   '((";foo"     . "foo")
     ("; foo"    . "foo")
     (";  foo"   . " foo")
     
     (";;foo"    . "foo")
     (";; foo"   . "foo")
     (";;  foo"  . " foo")
     
     (";;;foo"   . "foo")
     (";;; foo"  . "foo")
     (";;;  foo" . " foo"))))

(ert-deftest separedit-test-comment-delimiter-regexp-py ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (separedit--comment-delimiter-regexp 'python-mode) "" (car it)))))
   '(("#foo"     . "foo")
     ("# foo"    . "foo")
     ("#  foo"   . " foo")
     
     ("##foo"    . "foo")
     ("## foo"   . "foo")
     ("##  foo"  . " foo")
     
     ("###foo"   . "foo")
     ("### foo"  . "foo")
     ("###  foo" . " foo"))))

(ert-deftest separedit-test-comment-delimiter-regexp-c1 ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (separedit--comment-delimiter-regexp 'c-mode) "" (car it)))))
   '(("*foo"     . "foo")
     ("* foo"    . "foo")
     ("*  foo"   . " foo")

     ("**foo"    . "foo")
     ("** foo"   . "foo")
     ("**  foo"  . " foo")

     ("***foo"   . "foo")
     ("*** foo"  . "foo")
     ("***  foo" . " foo"))))

(ert-deftest separedit-test-comment-delimiter-regexp-c2 ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (separedit--comment-delimiter-regexp 'c-mode) "" (car it)))))
   '(("/foo"     . "/foo")
     ("/ foo"    . "/ foo")
     ("/  foo"   . "/  foo")

     ("//foo"    . "foo")
     ("// foo"   . "foo")
     ("//  foo"  . " foo")

     ("///foo"   . "foo")
     ("/// foo"  . "foo")
     ("///  foo" . " foo"))))

(ert-deftest separedit-test-string-region-el ()
  (add-hook 'prog-mode-hook 'xah-syntax-color-hex)
  (unwind-protect
    (let* ((content-string (format "%S" "string `symbol'\n#ffffff\n(function \"arg\")"))
           (expected-string (substring content-string 1 (1- (length content-string)))))
      (should (string= expected-string
                       (separedit-test--with-buffer 'emacs-lisp-mode
                        content-string
                        (apply #'buffer-substring-no-properties (separedit--string-region 20)))))
      (should (string= expected-string
                       (separedit-test--with-buffer 'emacs-lisp-mode
                        (concat "(" content-string ")")
                        (apply #'buffer-substring-no-properties (separedit--string-region 20)))))
      (should (string= expected-string
                       (separedit-test--with-buffer 'emacs-lisp-mode
                        (concat "(foo " content-string ")")
                        (apply #'buffer-substring-no-properties (separedit--string-region 20)))))
      (should (string= expected-string
                       (separedit-test--with-buffer 'emacs-lisp-mode
                        (concat "(defun foo () " content-string ")")
                        (apply #'buffer-substring-no-properties (separedit--string-region 20)))))
      (should (string= expected-string
                       (separedit-test--with-buffer 'emacs-lisp-mode
                        (concat "(defun foo () " content-string ")\n(foo)")
                        (apply #'buffer-substring-no-properties (separedit--string-region 20))))))
    (remove-hook 'prog-mode-hook 'xah-syntax-color-hex)))

(ert-deftest separedit-test-string-region-py ()
  (let* ((content-string "\"\"\"docstring & double quotes\"\"\"")
         (expected-string (substring content-string 3 (- (length content-string) 3))))
    (should (string= expected-string
                     (separedit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (separedit--string-region 20))))))
  (let* ((content-string "'''docstring & single quotes'''")
         (expected-string (substring content-string 3 (- (length content-string) 3))))
    (should (string= expected-string
                     (separedit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (separedit--string-region 20))))))
  (let* ((content-string "\"docstring & double quotes\"")
         (expected-string (substring content-string 1 (- (length content-string) 1))))
    (should (string= expected-string
                     (separedit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (separedit--string-region 20))))))
  (let* ((content-string "'docstring & single quotes'")
         (expected-string (substring content-string 1 (- (length content-string) 1))))
    (should (string= expected-string
                     (separedit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (separedit--string-region 20)))))))

(ert-deftest separedit-test-nested-escape ()
  (with-temp-buffer
    (separedit-double-quote-string-mode)
    (insert (escape "a" "b" "c" "d" "e"))
    (nest-and-assert
       (cons (escape "a" "b" "c" "d" "e") (escape "" "a" "b" "c" "d" "e"))
       (cons (escape "b" "c" "d" "e")     (escape "a" "b" "c" "d" "e"))
       (cons (escape "c" "d" "e")         (escape "b" "c" "d" "e"))
       (cons (escape "d" "e")             (escape "c" "d" "e"))
       (cons (escape "e")                 (escape "d" "e")))))

(ert-deftest separedit-test-nested-escape-sq ()
  (with-temp-buffer
    (separedit-single-quote-string-mode)
    (insert (escape-sq "a" "b" "c" "d" "e"))
    (nest-and-assert-sq
       (cons (escape-sq "a" "b" "c" "d" "e") (escape-sq "" "a" "b" "c" "d" "e"))
       (cons (escape-sq "b" "c" "d" "e")     (escape-sq "a" "b" "c" "d" "e"))
       (cons (escape-sq "c" "d" "e")         (escape-sq "b" "c" "d" "e"))
       (cons (escape-sq "d" "e")             (escape-sq "c" "d" "e"))
       (cons (escape-sq "e")                 (escape-sq "d" "e")))))

(ert-deftest separedit-test-point-between-two-code-blocks ()
  (separedit-test--with-buffer
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; ┌────
     ;; │ block2
     ;; └────
     ;;
     ;; <|>
     ;;
     ;; ┌────
     ;; │ block2
     ;; └────")
   (let* ((comment-beg (save-excursion (separedit--comment-beginning)))
          (comment-end (save-excursion (separedit--comment-beginning)))
          (edit-block  (save-excursion (separedit--block-info)))
          (block-beg    (plist-get edit-block :beginning))
          (block-end    (plist-get edit-block :beginning)))
     (should (and (eq comment-beg block-beg)
                  (eq comment-end block-end))))))

;;; Interaction test

(ert-deftest separedit-test-keybinding ()
  (require 'markdown-mode)
  (let ((init-str-d "\"string<|>\"")
        (init-str-s "'string<|>'")
        (init-block (separedit-test--indent-el
                     ";; comment1
           ;; comment2<|>
           ;; comment3")))
    (let ((separedit-default-mode 'fundamental-mode))
      (--with-callback 'emacs-lisp-mode init-block ""
                       (lambda () (should (--key= "C-c '"   'separedit
                                                  "C-c C-c" 'edit-indirect-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'separedit-double-quote-string-mode))
      (--with-callback 'javascript-mode init-str-d ""
                       (lambda () (should (--key= "C-c '"   'separedit
                                                  "C-c C-c" 'edit-indirect-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'separedit-single-quote-string-mode))
      (--with-callback 'javascript-mode init-str-s ""
                       (lambda () (should (--key= "C-c '"   'separedit
                                                  "C-c C-c" 'edit-indirect-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'org-mode))
      (--with-callback 'emacs-lisp-mode init-block ""
                       (lambda () (should (--key= "C-c '"   'org-edit-special
                                                  "C-c C-c" 'edit-indirect-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'markdown-mode))
      (--with-callback 'emacs-lisp-mode init-block ""
                       (lambda () (should (--key= "C-c '"   'markdown-edit-code-block
                                                  "C-c C-c" 'edit-indirect-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))))

(ert-deftest separedit-test-code-block-matching ()
  (let ((init-data (separedit-test--indent-c
                    "// ``` emacs-lisp
                     // (message \"foobar\")<|>
                     // ```")))
    (--with-callback 'c-mode init-data ""
                     (lambda () (and (--bufs= "(message \"foobar\")<|>")
                                     (--mode= 'emacs-lisp-mode)))))
  (let ((init-data (separedit-test--indent-el
                    ";; #+BEGIN_SRC c++
                     ;; console.log('foobar');<|>
                     ;; #+END_SRC")))
    (--with-callback 'emacs-lisp-mode init-data ""
                     (lambda () (and (--bufs= "console.log('foobar');<|>")
                                     (--mode= 'c++-mode)))))
  (let ((init-data (separedit-test--indent-c
                    "// ,--- emacs-lisp
                     // | (message \"foobar\")<|>
                     // `---")))
    (--with-callback 'c-mode init-data ""
                     (lambda () (and (--bufs= "(message \"foobar\")<|>")
                                     (--mode= 'emacs-lisp-mode)))))
  (let ((init-data (separedit-test--indent-el
                    ";; ┌──── c++
                     ;; │ cout << 'foobar';<|>
                     ;; └────")))
    (--with-callback 'emacs-lisp-mode init-data ""
                     (lambda () (and (--bufs= "cout << 'foobar';<|>")
                                     (--mode= 'c++-mode))))))

(ert-deftest separedit-test-el-in-el ()
  (let ((code-with-comment
         (separedit-test--indent-el
          "(defun sum (&rest nums)
             (funcall '+ nums))
           ;; ```elisp
           ;; (sum '(1 2 3)) ;; <|>
           ;; ;; => 6
           ;;```"))
        (code-in-editing
         (separedit-test--indent-el
          "(sum '(1 2 3)) ;; <|>
           ;; => 6")))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""            code-with-comment code-in-editing)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"     code-with-comment code-with-comment)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c C-c" code-with-comment (separedit-test--append-to-code-block
                                                                                             'emacs-lisp-mode code-with-comment "aaa"))))

(ert-deftest separedit-test-py-in-py ()
  (let ((code-with-comment
         (separedit-test--indent-py
          "def sum(*nums):
               sum = 0
               for n in nums:
                   sum = sum + n
               return sum
           # ```python
           # sum(1, 2, 3) # <|>
           # # => 6
           # ```"))
        (code-in-comment
         (separedit-test--indent-py
          "sum(1, 2, 3) # <|>
           # => 6")))
    (separedit-test--execute-block-edit 'python-mode ""            code-with-comment code-in-comment)
    (separedit-test--execute-block-edit 'python-mode "C-c C-c"     code-with-comment code-with-comment)
    (separedit-test--execute-block-edit 'python-mode "aaa C-c C-c" code-with-comment (separedit-test--append-to-code-block
                                                                                         'python-mode code-with-comment "aaa"))))

(ert-deftest separedit-test-rb-in-rb ()
  (let ((code-with-comment
         (separedit-test--indent-rb
          "def sum(*nums):
             nums.inject(0) {|sum,x| sum + x }
           # ```ruby
           # sum 1, 2, 3 # <|>
           # # => 6
           # ```"))
        (code-in-comment
         (separedit-test--indent-sh
          "sum 1, 2, 3 # <|>
           # => 6")))
    (separedit-test--execute-block-edit 'ruby-mode ""            code-with-comment code-in-comment)
    (separedit-test--execute-block-edit 'ruby-mode "C-c C-c"     code-with-comment code-with-comment)
    (separedit-test--execute-block-edit 'ruby-mode "aaa C-c C-c" code-with-comment (separedit-test--append-to-code-block
                                                                                       'ruby-mode code-with-comment "aaa"))))

(ert-deftest separedit-test-sh-in-c1 ()
  (let ((code-with-comment
         (separedit-test--indent-c
          "int main()
           {
             printf(\"Hellow, world!\");
             return 0;
           }
           /*
            * ```sh
            * # build <|>
            * make -k
            * ```
            */"))
        (code-in-comment
         (separedit-test--indent-sh
          "# build <|>
           make -k")))
    (separedit-test--execute-block-edit 'c-mode ""            code-with-comment code-in-comment)
    (separedit-test--execute-block-edit 'c-mode "C-c C-c"     code-with-comment code-with-comment)
    (separedit-test--execute-block-edit 'c-mode "aaa C-c C-c" code-with-comment (separedit-test--append-to-code-block
                                                                                    'c-mode code-with-comment "aaa"))))

(ert-deftest separedit-test-sh-in-c2 ()
  (let ((code-with-comment
         (separedit-test--indent-c
          "int main()
           {
             printf(\"Hellow, world!\");
             return 0;
           }
           //
           // ```sh
           // # build <|>
           // make -k
           // ```
           //"))
        (code-in-comment
         (separedit-test--indent-sh
          "# build <|>
           make -k")))
    (separedit-test--execute-block-edit 'c-mode ""            code-with-comment code-in-comment)
    (separedit-test--execute-block-edit 'c-mode "C-c C-c"     code-with-comment code-with-comment)
    (separedit-test--execute-block-edit 'c-mode "aaa C-c C-c" code-with-comment (separedit-test--append-to-code-block
                                                                                    'c-mode code-with-comment "aaa"))))

(ert-deftest separedit-test-code-in-doc-1 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ```elisp
    (hello \\\"foo\\\") ;; <|>
    ```\"
  (message \"hello, %s\" name))")
        (code-in-doc "(hello \"foo\") ;; <|>"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                init-data code-in-doc)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"         init-data init-data)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c C-c" init-data (separedit-test--append-to-code-block
                                                                                         'emacs-lisp-mode init-data "aaa"))))

(ert-deftest separedit-test-code-in-doc-2 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ,---elisp
    | (hello \\\"foo\\\") ;; <|>
    `---\"
  (message \"hello, %s\" name))")
        (code-in-doc "(hello \"foo\") ;; <|>"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                init-data code-in-doc)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"         init-data init-data)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c C-c" init-data (separedit-test--append-to-code-block
                                                                                         'emacs-lisp-mode init-data "aaa"))))

(ert-deftest separedit-test-code-in-doc-3 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:<|>

    ```elisp
    (hello \\\"foo\\\")
    ```\"
  (message \"hello, %s\" name))")
        (code-in-doc "Greet a person.

Usage:<|>

    ```elisp
    (hello \"foo\")
    ```"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                init-data code-in-doc)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"         init-data init-data)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c C-c" init-data (separedit-test--append-to-code-block
                                                                                         'emacs-lisp-mode init-data "aaa"))))

(ert-deftest separedit-test-indentable-docstring ()
  "Test indentable docstring.

  ┌────
  │ def function():
  │     '''
  │     Indentable
  │         docstring
  │     '''
  └────"
  (let ((init-str (--join\n "def function1():"
                            "    '''"
                            "    Docstring1"
                            "    Docstring1<|>"
                            "    Docstring1"
                            "    '''"
                            "    pass"))
        (edit-str (--join\n ""
                            "Docstring1"
                            "Docstring1<|>"
                            "Docstring1"
                            "    ")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "def function2():"
                            "    '''"
                            "    Docstring2"
                            "        Docstring2<|>"
                            "    Docstring2"
                            "    '''"
                            "    pass"))
        (edit-str (--join\n ""
                            "Docstring2"
                            "    Docstring2<|>"
                            "Docstring2"
                            "    ")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "def function3():"
                            "    '''Docstring3"
                            "    Docstring3<|>"
                            "    Docstring3"
                            "    '''"
                            "    pass"))
        (edit-str (--join\n "Docstring3"
                            "Docstring3<|>"
                            "Docstring3"
                            "    ")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-unindentable-docstring ()
  "Test unindentable docstring.

  ┌────
  │ def function():
  │     '''
  │     Unindentable
  │ docstring
  │     '''
  └────"
  (let ((init-str (--join\n "def function1():"
                            "    '''"
                            "  Docstring1"
                            "  Docstring1<|>"
                            "  Docstring1"
                            "    '''"
                            "    pass"))
        (edit-str (--join\n ""
                            "  Docstring1"
                            "  Docstring1<|>"
                            "  Docstring1"
                            "    ")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "def function2():"
                            "    '''"
                            "    Docstring2"
                            "Docstring2<|>"
                            "    Docstring2"
                            "    '''"
                            "    pass"))
        (edit-str (--join\n ""
                            "    Docstring2"
                            "Docstring2<|>"
                            "    Docstring2"
                            "    ")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "def function3():"
                            "    '''Docstring3"
                            "    Docstring3<|>"
                            "Docstring3"
                            "    '''"
                            "    pass"))
        (edit-str (--join\n "Docstring3"
                            "    Docstring3<|>"
                            "Docstring3"
                            "    ")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-string-escape-js ()
  (let* ((initial-string "'\"single quotes wrap<|> double quotes.\"'")
         (expected-string (substring initial-string 1 (- (length initial-string) 1))))
    (separedit-test--execute-block-edit 'javascript-mode ""        initial-string expected-string)
    (separedit-test--execute-block-edit 'javascript-mode "C-c C-c" initial-string initial-string))
  (let* ((initial-string "'\\'single quotes wrap<|> single quotes.\\''")
         (expected-string "'single quotes wrap<|> single quotes.'"))
    (separedit-test--execute-block-edit 'javascript-mode ""        initial-string expected-string)
    (separedit-test--execute-block-edit 'javascript-mode "C-c C-c" initial-string initial-string))
  (let* ((initial-string "\"'double quotes wrap<|> single quotes.'\"")
         (expected-string "'double quotes wrap<|> single quotes.'"))
    (separedit-test--execute-block-edit 'javascript-mode ""        initial-string expected-string)
    (separedit-test--execute-block-edit 'javascript-mode "C-c C-c" initial-string initial-string))
  (let* ((expected-string (format "%S" "nested double <|> quote string"))
         (initial-string (format "%S" expected-string)))
    (separedit-test--execute-block-edit 'javascript-mode ""        initial-string expected-string)))

(ert-deftest separedit-test-el-commentary ()
  (let* ((separedit-leave-blank-line-in-comment t)
         (initial-string "\
;;; Commentary:

;; comment1

;; comment2<|>

;; comment3

;;; Code:")
         (editing-string "\
comment1

comment2<|>

comment3

")
         (edit-string "\
;;; Commentary:

;; comment1

;; comment2<|>

;; comment3

;; aaa
;;; Code:")
         (initial-string2 (concat ";; comment0\n\n" initial-string "\n\n;; comment4")))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                initial-string  editing-string (list "^;;; Commentary:\n+" "^;;; .*:$"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                initial-string2 editing-string (list "^;;; Commentary:\n+" "^;;; .*:$"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"         initial-string  initial-string (list "^;;; Commentary:\n+" "^;;; .*:$"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-j C-c C-c" initial-string  edit-string    (list "^;;; Commentary:\n+" "^;;; .*:$"))))

(ert-deftest separedit-test-readme ()
  (cl-assert (string= (separedit-test--generate-readme)
                      (with-temp-buffer
                        (insert-file-contents "README.md")
                        (goto-char (point-min))
                        (re-search-forward "\n\n") ;; skip notice
                        (buffer-substring (point) (point-max))))
             nil
             ">>> The README.md must be generated from commentrary <<<"))

(provide 'separedit-test)

;;; separedit-test.el ends here
