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

(ert-deftest separedit-test-region-of-comment-c4 ()
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "// comment
     // comment
     // comment

     // comment1
     // comment2<|>
     // comment3

     // comment
     // comment
     // comment")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^// comment1$" nil t))
           (save-excursion (re-search-forward "^// comment3$" nil t))))))
  (separedit-test--with-buffer
   'c-mode
   (separedit-test--indent-c
    "/*
      * comment
      */

     // comment1
     // comment2<|>
     // comment3

     /*
      * comment
      */")
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion (re-search-backward "^// comment1$" nil t))
           (save-excursion (re-search-forward "^// comment3$" nil t)))))))

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

(ert-deftest separedit-test-region-of-c-style-banner-comment ()
  (separedit-test--with-buffer
   'c-mode
   "\
/*******************************************************************************
 * A brief history of JavaDoc-style (C-style) banner comments.
 *
 * This is the typical JavaDoc-style C-style 'banner' comment. It starts with
 * a forward slash followed by some number, n, of asterisks, where n > 2. It's
 * written this way to be more 'visible' to developers who are reading the
 * source code.
 *
 * Often, developers are unaware that this is not (by default) a valid Doxygen
 * comment block!
 *
 * However, as long as JAVADOC_BLOCK = YES is added to the Doxyfile, it will
 * work as expected.
 *
 * This style of commenting behaves well with clang-format.
 *
 * @param theory Even if there is only one possible unified theory. it is just a
 *               set of rules and equations.
 ******************************************************************************/"
   (should
    (equal
     (separedit--comment-region)
     (list (save-excursion
             (goto-char (point-min)) (re-search-forward "/\\*+\n"))
           (save-excursion
             (goto-char (point-max)) (re-search-backward "\n\s+\\*+/")))))))

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

(ert-deftest separedit-test-help-value-edit-info ()
  (--assert-help-value
   "tooltip-hook is a variable defined in ‘tooltip.el’.
Its value is (tooltip-help<|>-tips)

  This variable is an alias for ‘tooltip-functions’.
  This variable is obsolete since 23.1;
  use ‘tooltip-functions’ instead.
  This variable may be risky if used as a file-local variable.

Documentation:
Functions to call to display tooltips.
Each function is called with one argument EVENT which is a copy
of the last mouse movement event that occurred.  If one of these
functions displays the tooltip, it should return non-nil and the
rest are not called.

[back]"
   '(tooltip-hook "(tooltip-help<|>-tips)" global nil))
  (--assert-help-value
   "company-backends1 is a variable defined in ‘company.el’.
Its value is shown below.

  This variable is safe as a file local variable if its value
  satisfies the predicate ‘company-safe-backends-p’.
  You can customize this variable.

Documentation:
The list of active backends (completion engines).

......

Value: (company-lsp company-capf<|>)
Original value was 
(company-cmake company-capf company-files company-oddmuse company-dabbrev)
Local in buffer *.py; global value is 
(company-cmake company-capf company-files company-oddmuse company-dabbrev)

[back]"
   '(company-backends1 "(company-lsp company-capf<|>)" local "*.py"))
  (--assert-help-value
   "company-backends2 is a variable defined in ‘company.el’.
Its value is shown below.

  This variable is safe as a file local variable if its value
  satisfies the predicate ‘company-safe-backends-p’.
  You can customize this variable.

Documentation:
The list of active backends (completion engines).

......

Value: (company-lsp company-capf)
Original value was 
(company-cmake company-capf<|> company-files company-oddmuse company-dabbrev)
Local in buffer *.py; global value is 
(company-cmake company-capf company-files company-oddmuse company-dabbrev)

[back]"
   '(company-backends2 "(company-cmake company-capf<|> company-files company-oddmuse company-dabbrev)" global "*.py"))
  (--assert-help-value
   "company-backends3 is a variable defined in ‘company.el’.
Its value is shown below.

  This variable is safe as a file local variable if its value
  satisfies the predicate ‘company-safe-backends-p’.
  You can customize this variable.

Documentation:
The list of active backends (completion engines).

......

Value: (company-lsp company-capf)
Original value was 
(company-cmake company-capf company-files company-oddmuse company-dabbrev)
Local in buffer *.py; global value is 
(company-cmake company-capf<|> company-files company-oddmuse company-dabbrev)

[back]"
   '(company-backends3 "(company-cmake company-capf<|> company-files company-oddmuse company-dabbrev)" global "*.py")))

(ert-deftest separedit-test-helpful-value-edit-info ()
  (--assert-helpful-value
   "tooltip-hook is an alias for tooltip-functions, defined in
tooltip.el.gz.

This variable is obsolete since 23.1; use tooltip-functions instead.

Value
(tooltip-help<|>-tips)

View as literal Set

Documentation
Functions to call to display tooltips.

..."
   '(tooltip-hook "(tooltip-help<|>-tips)" global nil))
  (--assert-helpful-value
   "company-backends1 is a variable defined in company.el.

Value in #<buffer *.py>
(company-lsp company-capf<|>)

Original Value
(company-cmake company-capf company-files company-oddmuse company-dabbrev)

Set Customize

Documentation
The list of active backends (completion engines).

..."
   '(company-backends1 "(company-lsp company-capf<|>)" local "*.py"))
  (--assert-helpful-value
   "company-backends2 is a variable defined in company.el.

Value in #<buffer *.py>
(company-lsp company-capf)

Original Value
(company-cmake company-capf<|> company-files company-oddmuse company-dabbrev)

Set Customize

Documentation
The list of active backends (completion engines).

..."
   '(company-backends2 "(company-cmake company-capf<|> company-files company-oddmuse company-dabbrev)" global nil)))

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
                                                  "C-c C-c" 'separedit-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'separedit-double-quote-string-mode))
      (--with-callback 'javascript-mode init-str-d ""
                       (lambda () (should (--key= "C-c '"   'separedit
                                                  "C-c C-c" 'separedit-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'separedit-single-quote-string-mode))
      (--with-callback 'javascript-mode init-str-s ""
                       (lambda () (should (--key= "C-c '"   'separedit
                                                  "C-c C-c" 'separedit-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'org-mode))
      (--with-callback 'emacs-lisp-mode init-block ""
                       (lambda () (should (--key= "C-c '"   'org-edit-special
                                                  "C-c C-c" 'separedit-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'markdown-mode))
      (--with-callback 'emacs-lisp-mode init-block ""
                       (lambda () (should (--key= "C-c '"   'markdown-edit-code-block
                                                  "C-c C-c" 'separedit-commit
                                                  "C-c C-k" 'edit-indirect-abort)))))))

(ert-deftest separedit-test-text-mode-keybinding ()
  (let ((init-str (--join\n "// # xml<|>"
                            "// "
                            "// ``` nxml"
                            "// <root></root>"
                            "// ```"))
        (edit-str (--join\n "# xml<|>"
                            ""
                            "``` nxml"
                            "<root></root>"
                            "```")))
    (let ((separedit-default-mode 'text-mode))
      (--with-callback 'c-mode init-str "" (lambda ()
                                             (should (--key= "C-c '"   'separedit))
                                             (should (--key= "C-c C-c" 'separedit-commit))
                                             (should (--key= "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'markdown-mode))
      (--with-callback 'c-mode init-str "" (lambda ()
                                             (should (--key= "C-c '"   'markdown-edit-code-block))
                                             (should (--key= "C-c C-c" 'separedit-commit))
                                             (should (--key= "C-c C-k" 'edit-indirect-abort))))))
  (let ((init-str (--join\n "// # xml"
                            "// "
                            "// ``` nxml"
                            "// <root><|></root>"
                            "// ```"))
        (edit-str "<root><|></root>"))
    (let ((separedit-default-mode 'text-mode))
      (--with-callback 'c-mode init-str "" (lambda ()
                                             (should (--key= "C-c '"   'separedit))
                                             (should (--key= "C-c C-c" 'separedit-commit))
                                             (should (--key= "C-c C-k" 'edit-indirect-abort)))))
    (let ((separedit-default-mode 'markdown-mode))
      (--with-callback 'c-mode init-str "" (lambda ()
                                             (should (--key= "C-c '"   'separedit))
                                             (should (--key= "C-c C-c" 'separedit-commit))
                                             (should (--key= "C-c C-k" 'edit-indirect-abort)))))))

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
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c C-c" code-with-comment (replace-regexp-in-string
                                                                                          "<|>" "<|>aaa" code-with-comment))))

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
    (separedit-test--execute-block-edit 'python-mode "aaa C-c C-c" code-with-comment (replace-regexp-in-string
                                                                                      "<|>" "<|>aaa" code-with-comment))))

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
    (separedit-test--execute-block-edit 'ruby-mode "aaa C-c C-c" code-with-comment (replace-regexp-in-string
                                                                                    "<|>" "<|>aaa" code-with-comment))))

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
    (separedit-test--execute-block-edit 'c-mode "aaa C-c C-c" code-with-comment (replace-regexp-in-string
                                                                                 "<|>" "<|>aaa" code-with-comment))))

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
    (separedit-test--execute-block-edit 'c-mode "aaa C-c C-c" code-with-comment (replace-regexp-in-string
                                                                                 "<|>" "<|>aaa" code-with-comment))))

(ert-deftest separedit-test-code-in-doc-1 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ```elisp
    (let ((name \\\"foo\\\"))
      (hello name)) ;; <|>
    ```\"
  (message \"hello, %s\" name))")
        (code-in-doc "\
(let ((name \"foo\"))
  (hello name)) ;; <|>"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                init-data code-in-doc)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"         init-data init-data)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c C-c"     init-data (replace-regexp-in-string
                                                                                      "<|>" "<|>aaa" init-data))))

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
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c C-c"     init-data (replace-regexp-in-string
                                                                                      "<|>" "<|>aaa" init-data))))

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
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c C-c"     init-data (replace-regexp-in-string
                                                                                      "<|>" "<|>aaa" init-data))))

(ert-deftest separedit-test-code-in-doc-4-no-indent ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

,---elisp
| (let ((name \\\"foo\\\")) ;;<|>
|   (hello name))
`---\"
  (message \"hello, %s\" name))")
        (code-in-doc "\
(let ((name \"foo\")) ;;<|>
  (hello name))"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode ""                init-data code-in-doc)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "C-c C-c"         init-data init-data)
    (separedit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c C-c"     init-data (replace-regexp-in-string
                                                                                      "<|>" "<|>aaa" init-data))))

(ert-deftest separedit-test-code-in-comment ()
  ;; with indent
  (let ((init-data "\
;; # Code block with blank line
;;
;;   ``` elisp
;;   (foo) <|>
;;
;;   (bar)
;;   ```")
        (code-in-doc "\
(foo) <|>

(bar)"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode "" init-data code-in-doc))
  ;; without indent
  (let ((init-data "\
;; # Code block with blank line
;;
;; ``` elisp
;; (foo) <|>
;;
;; (bar)
;; ```")
        (code-in-doc "\
(foo) <|>

(bar)"))
    (separedit-test--execute-block-edit 'emacs-lisp-mode "" init-data code-in-doc)))

(ert-deftest separedit-test-preserve-string-indent-1 ()
  "String block with both of STAR & END quotes at a new line"
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    '''"
                            "    String block 11"
                            "    String block 11<|>"
                            "    String block 11"
                            "    '''"))
        (edit-str (--join\n "String block 11"
                            "String block 11<|>"
                            "String block 11")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    '''"
                            "    String block 12"
                            "        String block 12<|>"
                            "    String block 12"
                            "    '''"))
        (edit-str (--join\n "String block 12"
                            "    String block 12<|>"
                            "String block 12")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    '''String block 13"
                            "       String block 13<|>"
                            "       String block 13"
                            "    '''"))
        (edit-str (--join\n "String block 13"
                            "String block 13<|>"
                            "String block 13")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))

  ;; Make sure ‘separedit-string-indent-offset-alist’ not act on this case
  (let ((separedit-preserve-string-indentation t)
        (separedit-string-indent-offset-alist '((python-mode . 2)))
        (init-str (--join\n "    '''"
                            "      String block 11"
                            "      String block 11<|>"
                            "      String block 11"
                            "    '''"))
        (edit-str (--join\n "  String block 11"
                            "  String block 11<|>"
                            "  String block 11")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-preserve-string-indent-2 ()
  "String block with only END quotes at a new line"
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    str = '''"
                            "    String block 21"
                            "    String block 21<|>"
                            "    String block 21"
                            "    '''"))
        (edit-str (--join\n "String block 21"
                            "String block 21<|>"
                            "String block 21")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    str = '''"
                            "    String block 22"
                            "        String block 22<|>"
                            "    String block 22"
                            "    '''"))
        (edit-str (--join\n "String block 22"
                            "    String block 22<|>"
                            "String block 22")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    str = '''String block 23"
                            "             String block 23<|>"
                            "             String block 23"
                            "    '''"))
        (edit-str (--join\n "String block 23"
                            "String block 23<|>"
                            "String block 23")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))

  ;; Make sure ‘separedit-string-indent-offset-alist’ act on this case
  (let ((separedit-preserve-string-indentation t)
        (separedit-string-indent-offset-alist '((python-mode . 2)))
        (init-str (--join\n "    str = '''"
                            "      String block 21"
                            "      String block 21<|>"
                            "      String block 21"
                            "    '''"))
        (edit-str (--join\n "String block 21"
                            "String block 21<|>"
                            "String block 21")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-preserve-string-indent-3 ()
  "String block with both of START & END quotes NOT at a new line"
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "emacs --batch --eval '(progn"
                            "                        (+ 1"
                            "                           2 ;;<|>"
                            "                           3))'"))
        (edit-str (--join\n "(progn"
                            "  (+ 1"
                            "     2 ;;<|>"
                            "     3))")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-dont-preserve-string-indent ()
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    '''"
                            "  Dont preserve indent 1"
                            "  Dont preserve indent 1<|>"
                            "  Dont preserve indent 1"
                            "    '''"))
        (edit-str (--join\n "  Dont preserve indent 1"
                            "  Dont preserve indent 1<|>"
                            "  Dont preserve indent 1")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    '''"
                            "    Dont preserve indent 2"
                            "Dont preserve indent 2<|>"
                            "    Dont preserve indent 2"
                            "    '''"))
        (edit-str (--join\n "    Dont preserve indent 2"
                            "Dont preserve indent 2<|>"
                            "    Dont preserve indent 2")))
    (--with-callback 'python-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'python-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((separedit-preserve-string-indentation t)
        (init-str (--join\n "    '''Dont preserve indent 3"
                            "    Dont preserve indent 3<|>"
                            "Dont preserve indent 3"
                            "    '''"))
        (edit-str (--join\n "Dont preserve indent 3"
                            "    Dont preserve indent 3<|>"
                            "Dont preserve indent 3")))
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

;; comment2<|>aaa


;; comment3

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

(ert-deftest separedit-test-retain-point ()
  (--with-callback
   'emacs-lisp-mode
   (separedit-test--indent-el
    "\"retain
       point <|>
       in string\"")
   ""
   (lambda ()
     (should (equal (point)
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "<|>" nil t))))))
  (--with-callback
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; retain <|>
     ;; point
     ;; in comment")
   ""
   (lambda ()
     (should (equal (point)
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "<|>" nil t))))))

(--with-callback
   'emacs-lisp-mode
   (separedit-test--indent-el
    ";; ```
     ;; (retain
     ;;  (point
     ;;   in codeblock <|>)
     ;; ```")
   ""
   (lambda ()
     (should (equal (point)
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "<|>" nil t)))))))

(ert-deftest separedit-test-auto-mode-alist-value-bound ()
  "Seems there are something makes `syntax-ppss' wrong in the doc."
  (test-with '(call-interactively #'describe-variable) "auto-mode-alist RET")
  (switch-to-buffer "*Help*")
  (goto-char (point-min))
  (search-forward "tar-mode")
  (let ((bound (separedit-described-value-bound)))
    (should (eq 'tar-mode
                (cdr (assoc "\\.txz\\'" (car (read-from-string
                                              (buffer-substring-no-properties
                                               (car bound)
                                               (cdr bound))))))))))

(ert-deftest separedit-test-gnu-style-comment ()
  (let ((init-str (--join\n "/* comment 1"
                            "   comment 2<|>"
                            "   comment 3 */"))
        (edit-str (--join\n "comment 1"
                            "comment 2<|>"
                            "comment 3")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "{ comment 4"
                            "  comment 5<|>"
                            "  comment 6 }"))
        (edit-str (--join\n "comment 4"
                            "comment 5<|>"
                            "comment 6")))
    (--with-callback 'pascal-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'pascal-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-c-style-doc-comment ()
  (let ((init-str (--join\n "/** comment 1"
                            " *  comment 2<|>"
                            " *  comment 3"
                            " */"))
        (edit-str (--join\n "comment 1"
                            "comment 2<|>"
                            "comment 3")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "/* comment 4"
                            " * comment 5<|>"
                            " * comment 6"
                            " */"))
        (edit-str (--join\n "comment 4"
                            "comment 5<|>"
                            "comment 6")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "/**"
                            " * comment 7"
                            " * comment 8<|>"
                            " * comment 9"
                            " */"))
        (edit-str (--join\n "comment 7"
                            "comment 8<|>"
                            "comment 9")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-c-style-comment-indent ()
  (let ((init-str (--join\n "/** comment 1"
                            "    comment 2<|>"
                            "    comment 3"
                            " */"))
        (edit-str (--join\n "comment 1"
                            "comment 2<|>"
                            "comment 3")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "/**"
                            "    comment 4"
                            "    comment 5<|>"
                            "    comment 6"
                            " */"))
        (edit-str (--join\n "comment 4"
                            "comment 5<|>"
                            "comment 6")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  )

(ert-deftest separedit-test-doxygen-style-c-comment ()
  (let ((init-str (--join\n "/**"
                            " * comment11<|>"
                            " */"))
        (edit-str "comment11<|>"))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "/*!"
                            " * comment12<|>"
                            " */"))
        (edit-str "comment12<|>"))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "/*!"
                            "   comment13<|>"
                            " */"))
        (edit-str "comment13<|>"))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "/*! \file structcmd.h"
                            "    \brief A documented file."
                            "    Details<|>"
                            " */"))
        (edit-str (--join\n "\file structcmd.h"
                            "\brief A documented file."
                            "Details<|>")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))

  ;;;

  (let ((init-str (--join\n "/// comment21"
                            "/// comment21<|>"
                            "/// comment21"))
        (edit-str (--join\n "comment21"
                            "comment21<|>"
                            "comment21")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "//! comment22"
                            "//! comment22<|>"
                            "//! comment22"))
        (edit-str (--join\n "comment22"
                            "comment22<|>"
                            "comment22")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "//!< comment23"
                            "//!< comment23<|>"
                            "//!< comment23"))
        (edit-str (--join\n "comment23"
                            "comment23<|>"
                            "comment23")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str)))))
  (let ((init-str (--join\n "///< comment24"
                            "///< comment24<|>"
                            "///< comment24"))
        (edit-str (--join\n "comment24"
                            "comment24<|>"
                            "comment24")))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-doxygen-style-c-comment-issue24 ()
  (let ((separedit-remove-trailing-spaces-in-comment t)
        (init-str "\
/**
 * @brief This the entry for this program.<|>
 *
 * Detailed description goes here. And it's long enough
 * to span multiple lines.
 *
 * @param argc Count of arguments.
 * @param argv Arguments array.
 */
int main(int argc, char *argv[])
{
     return 0;
}")
        (edit-str "\
@brief This the entry for this program.<|>

Detailed description goes here. And it's long enough
to span multiple lines.

@param argc Count of arguments.
@param argv Arguments array."))
    (--with-callback 'c-mode init-str ""        (lambda () (should (--bufs= edit-str))))
    (--with-callback 'c-mode init-str "C-c C-c" (lambda () (should (--bufs= init-str))))))

(ert-deftest separedit-test-local-fill-column ()
  (let ((dir-local-fill-column (+ fill-column 10))
        (buf-local-fill-column (+ fill-column 20))
        (default-directory (make-temp-file "test-fill-column--" 'tmpdir "/"))
        (comment-prefix "// "))
    (with-temp-buffer
      (insert (format "%s" `((nil . ((fill-column . ,dir-local-fill-column))))))
      (write-region (point-min) (point-max) ".dir-locals.el"))
    (with-current-buffer (find-file-noselect "main.c")
      (when (= 25.1 (string-to-number emacs-version))
        ;; local variables will lose efficasy after c-mode enabled on 25.1
        (add-hook 'c-mode-hook #'hack-local-variables-apply))
      (c-mode)
      (insert (concat comment-prefix "comment<|>"))
      (let ((noninteractive nil))
        (font-lock-mode 1)
        (unless (and (= 25.1 (string-to-number emacs-version))
                     (memq major-mode '(python-mode)))
          (font-lock-ensure)))
      ;;
      ;; dir local fill-column
      ;;
      (save-excursion
        (goto-char (point-max))
        (separedit)
        (should (= fill-column dir-local-fill-column))
        (test-with nil "C-c C-k"))
      (save-excursion
        (let ((separedit-continue-fill-column t))
          (goto-char (point-max))
          (separedit)
          (should (= fill-column (- dir-local-fill-column (length comment-prefix))))
          (test-with nil "C-c C-k")))
      ;;
      ;; buffer local fill-column
      ;;
      (save-excursion
        (setq-local fill-column buf-local-fill-column)
        (goto-char (point-max))
        (separedit)
        (should (= fill-column buf-local-fill-column))
        (test-with nil "C-c C-k"))
      (save-excursion
        (let ((separedit-continue-fill-column t))
          (setq-local fill-column buf-local-fill-column)
          (goto-char (point-max))
          (separedit)
          (should (= fill-column (- buf-local-fill-column (length comment-prefix))))
          (test-with nil "C-c C-k"))))))

(provide 'separedit-test)

;;; separedit-test.el ends here
