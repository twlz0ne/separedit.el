;;; comment-edit-test.el --- Test comment-edit -*- lexical-binding: t; -*-

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

(require 'ert)
;; (setq ert-batch-backtrace-right-margin nil)

(require 'comment-edit)
;; (comment-edit-toggle-debug t)

(when noninteractive
  (transient-mark-mode))

;;; Function test

(ert-deftest comment-edit-test-beginning-of-comment-el ()
  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3")
   (should (eq (comment-edit--comment-beginning) (+ (point-min) 0))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "
     ;; comment1
     ;; comment2<|>
     ;; comment3")
   (should (eq (comment-edit--comment-beginning) (+ (point-min) 1))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "\n
     ;; comment1
     ;; comment2<|>
     ;; comment3\n\n")
   (should (eq (comment-edit--comment-beginning) (+ (point-min) 2))))

  ;;;
  
  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3")
   (should (eq (comment-edit--comment-beginning) (+ (point-min) 0))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "
           ;; comment1
           ;; comment2<|>
           ;; comment3\n")
   (should (eq (comment-edit--comment-beginning) (+ (point-min) 1))))
  
  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "\n
         ;; comment1
          ;; comment2<|>
          ;; comment3\n\n")
   (should (eq (comment-edit--comment-beginning) (+ (point-min) 2)))))

(ert-deftest comment-edit-test-end-of-comment-el ()
  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3")
   (should (eq (comment-edit--comment-end) (- (point-max) 0))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3\n")
   (should (eq (comment-edit--comment-end) (- (point-max) 1))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3\n\n")
   (should (eq (comment-edit--comment-end) (- (point-max) 2))))

  ;;;

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3")
   (should (eq (comment-edit--comment-end) (- (point-max) 0))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3\n")
   (should (eq (comment-edit--comment-end) (- (point-max) 1))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    "     ;; comment1
          ;; comment2<|>
          ;; comment3\n\n")
   (should (eq (comment-edit--comment-end) (- (point-max) 2))))

  ;;; With trailing empty comment line

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3
     ;;")
   (should (eq (comment-edit--comment-end) (- (point-max) 0))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3
     ;; ")
   (should (eq (comment-edit--comment-end) (- (point-max) 0))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   (comment-edit-test--indent-el
    ";; comment1
     ;; comment2<|>
     ;; comment3
     ;;\n")
   (should (eq (comment-edit--comment-end) (- (point-max) 1)))))

(ert-deftest comment-edit-test-region-of-comment-c1 ()
  ;;; Without leading spaces
  
  ;; Without blank lines

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (point-max)))))

  ;; Blank lines at the end
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (- (point-max) 1)))))
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3\n\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (- (point-max) 2)))))

  ;; Blank lines at the bebinning

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n"
    "// comment1
     // comment2<|>
     // comment3")
   (should
    (equal
     (comment-edit--comment-region)
     (list (+ (point-min) 1)
           (point-max)))))
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n\n"
    "// comment1
     // comment2<|>
     // comment3")
   (should
    (equal
     (comment-edit--comment-region)
     (list (+ (point-min) 2)
           (point-max)))))

  ;;; With leading spaces

  ;; Without blank lines

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "   // comment1
        // comment2<|>
        // comment3")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (point-max)))))

  ;; Blank lines at the end

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "   // comment1
        // comment2<|>
        // comment3\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (- (point-max) 1)))))

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "   // comment1
        // comment2<|>
        // comment3\n\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (- (point-max) 2)))))

  ;; Blank lines at the end

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n"
    "   // comment1
        // comment2<|>
        // comment3")
   (should
    (equal
     (comment-edit--comment-region)
     (list (+ (point-min) 1)
           (point-max)))))

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n\n"
    "   // comment1
        // comment2<|>
        // comment3")
   (should
    (equal
     (comment-edit--comment-region)
     (list (+ (point-min) 2)
           (point-max)))))

  ;;; With trailing empty comment line

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3
     //")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (point-max)))))

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3
     // ")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (point-max)))))

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "// comment1
     // comment2<|>
     // comment3
     //\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (point-min)
           (- (point-max) 1))))))

(ert-deftest comment-edit-test-region-of-comment-c2 ()
  ;;; Without leading spaces

  ;; Without blank lines
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "/*
      * comment1
      * comment2<|>
      * comment3
      */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the end
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "/*
      * comment1
      * comment2<|>
      * comment3
      */\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "/*
      * comment1
      * comment2<|>
      * comment3
      */\n\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the beginning
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n"
    "/*
      * comment1
      * comment2<|>
      * comment3
      */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))
  
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n\n"
    "/*
      * comment1
      * comment2<|>
      * comment3
      */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;;; With leading spaces

  ;; Without blank lines

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the end

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */\n\n")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  ;; Blank lines at the beginning

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n"
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t))))))

  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "\n\n"
    "   /*
         * comment1
         * comment2<|>
         * comment3
         */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*\\* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*\\* comment3$" nil t)))))))

(ert-deftest comment-edit-test-region-of-comment-c3 ()
  (comment-edit-test--with-buffer
   'c-mode
   (comment-edit-test--indent-c
    "/*
        comment1
        comment2<|>
        comment3
      */")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-* comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-* comment3$" nil t)))))))

(ert-deftest comment-edit-test-region-of-comment-pascal ()
  (comment-edit-test--with-buffer
   'pascal-mode
   (comment-edit-test--indent-pascal
    "{
     comment1
     comment2<|>
     comment3
     }")
   (should
    (equal
     (comment-edit--comment-region)
     (list (save-excursion (re-search-backward "^\\s-*comment1$" nil t))
           (save-excursion (re-search-forward "^\\s-*comment3$" nil t)))))
   ))

(ert-deftest comment-edit-test-comment-at-end-of-comment ()
  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   ";; comment-without-trailing-spaces"
   (should (not (comment-edit--point-at-comment (point-max)))))

  (comment-edit-test--with-buffer
   'emacs-lisp-mode
   ";; comment-with-trailing-spaces "
   (should (not (comment-edit--point-at-comment (point-max))))))

(ert-deftest comment-edit-test-comment-delimiter-regexp-el ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (comment-edit--comment-delimiter-regexp 'emacs-lisp-mode) "" (car it)))))
   '((";foo"     . "foo")
     ("; foo"    . "foo")
     (";  foo"   . " foo")
     
     (";;foo"    . "foo")
     (";; foo"   . "foo")
     (";;  foo"  . " foo")
     
     (";;;foo"   . "foo")
     (";;; foo"  . "foo")
     (";;;  foo" . " foo"))))

(ert-deftest comment-edit-test-comment-delimiter-regexp-py ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (comment-edit--comment-delimiter-regexp 'python-mode) "" (car it)))))
   '(("#foo"     . "foo")
     ("# foo"    . "foo")
     ("#  foo"   . " foo")
     
     ("##foo"    . "foo")
     ("## foo"   . "foo")
     ("##  foo"  . " foo")
     
     ("###foo"   . "foo")
     ("### foo"  . "foo")
     ("###  foo" . " foo"))))

(ert-deftest comment-edit-test-comment-delimiter-regexp-c1 ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (comment-edit--comment-delimiter-regexp 'c-mode) "" (car it)))))
   '(("*foo"     . "foo")
     ("* foo"    . "foo")
     ("*  foo"   . " foo")

     ("**foo"    . "foo")
     ("** foo"   . "foo")
     ("**  foo"  . " foo")

     ("***foo"   . "foo")
     ("*** foo"  . "foo")
     ("***  foo" . " foo"))))

(ert-deftest comment-edit-test-comment-delimiter-regexp-c2 ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (comment-edit--comment-delimiter-regexp 'c-mode) "" (car it)))))
   '(("/foo"     . "/foo")
     ("/ foo"    . "/ foo")
     ("/  foo"   . "/  foo")

     ("//foo"    . "foo")
     ("// foo"   . "foo")
     ("//  foo"  . " foo")

     ("///foo"   . "foo")
     ("/// foo"  . "foo")
     ("///  foo" . " foo"))))

(ert-deftest comment-edit-test-string-region-el ()
  (add-hook 'prog-mode-hook 'xah-syntax-color-hex)
  (unwind-protect
    (let* ((content-string (format "%S" "string `symbol'\n#ffffff\n(function \"arg\")"))
           (expected-string (substring content-string 1 (1- (length content-string)))))
      (should (string= expected-string
                       (comment-edit-test--with-buffer 'emacs-lisp-mode
                        content-string
                        (apply #'buffer-substring-no-properties (comment-edit--string-region 20)))))
      (should (string= expected-string
                       (comment-edit-test--with-buffer 'emacs-lisp-mode
                        (concat "(" content-string ")")
                        (apply #'buffer-substring-no-properties (comment-edit--string-region 20)))))
      (should (string= expected-string
                       (comment-edit-test--with-buffer 'emacs-lisp-mode
                        (concat "(foo " content-string ")")
                        (apply #'buffer-substring-no-properties (comment-edit--string-region 20)))))
      (should (string= expected-string
                       (comment-edit-test--with-buffer 'emacs-lisp-mode
                        (concat "(defun foo () " content-string ")")
                        (apply #'buffer-substring-no-properties (comment-edit--string-region 20)))))
      (should (string= expected-string
                       (comment-edit-test--with-buffer 'emacs-lisp-mode
                        (concat "(defun foo () " content-string ")\n(foo)")
                        (apply #'buffer-substring-no-properties (comment-edit--string-region 20))))))
    (remove-hook 'prog-mode-hook 'xah-syntax-color-hex)))

(ert-deftest comment-edit-test-string-region-py ()
  (let* ((content-string "\"\"\"docstring & double quotes\"\"\"")
         (expected-string (substring content-string 3 (- (length content-string) 3))))
    (should (string= expected-string
                     (comment-edit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (comment-edit--string-region 20))))))
  (let* ((content-string "'''docstring & single quotes'''")
         (expected-string (substring content-string 3 (- (length content-string) 3))))
    (should (string= expected-string
                     (comment-edit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (comment-edit--string-region 20))))))
  (let* ((content-string "\"docstring & double quotes\"")
         (expected-string (substring content-string 1 (- (length content-string) 1))))
    (should (string= expected-string
                     (comment-edit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (comment-edit--string-region 20))))))
  (let* ((content-string "'docstring & single quotes'")
         (expected-string (substring content-string 1 (- (length content-string) 1))))
    (should (string= expected-string
                     (comment-edit-test--with-buffer 'python-mode
                      content-string
                      (apply #'buffer-substring-no-properties (comment-edit--string-region 20)))))))

(ert-deftest comment-edit-test-nested-escape ()
  (with-temp-buffer
    (comment-edit-double-quote-string-mode)
    (insert (escape "a" "b" "c" "d" "e"))
    (nest-and-assert
       (cons (escape "a" "b" "c" "d" "e") (escape "" "a" "b" "c" "d" "e"))
       (cons (escape "b" "c" "d" "e")     (escape "a" "b" "c" "d" "e"))
       (cons (escape "c" "d" "e")         (escape "b" "c" "d" "e"))
       (cons (escape "d" "e")             (escape "c" "d" "e"))
       (cons (escape "e")                 (escape "d" "e")))))

(ert-deftest comment-edit-test-nested-escape-sq ()
  (with-temp-buffer
    (comment-edit-single-quote-string-mode)
    (insert (escape-sq "a" "b" "c" "d" "e"))
    (nest-and-assert-sq
       (cons (escape-sq "a" "b" "c" "d" "e") (escape-sq "" "a" "b" "c" "d" "e"))
       (cons (escape-sq "b" "c" "d" "e")     (escape-sq "a" "b" "c" "d" "e"))
       (cons (escape-sq "c" "d" "e")         (escape-sq "b" "c" "d" "e"))
       (cons (escape-sq "d" "e")             (escape-sq "c" "d" "e"))
       (cons (escape-sq "e")                 (escape-sq "d" "e")))))

;;; Interaction test

(ert-deftest comment-edit-test-el-in-el ()
  (let ((code-with-comment
         (comment-edit-test--indent-el
          "(defun sum (&rest nums)
             (funcall '+ nums))
           ;; ```elisp
           ;; (sum '(1 2 3)) ;; <|>
           ;; ;; => 6
           ;;```"))
        (code-in-editing
         (comment-edit-test--indent-el
          "(sum '(1 2 3)) ;; <|>
           ;; => 6")))
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode ""           code-with-comment code-in-editing)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "C-c '"      code-with-comment code-with-comment)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "aaa C-c '"  code-with-comment (comment-edit-test--append-to-code-block
                                                                                           'emacs-lisp-mode code-with-comment "aaa"))))

(ert-deftest comment-edit-test-py-in-py ()
  (let ((code-with-comment
         (comment-edit-test--indent-py
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
         (comment-edit-test--indent-py
          "sum(1, 2, 3) # <|>
           # => 6")))
    (comment-edit-test--execute-block-edit 'python-mode ""          code-with-comment code-in-comment)
    (comment-edit-test--execute-block-edit 'python-mode "C-c '"     code-with-comment code-with-comment)
    (comment-edit-test--execute-block-edit 'python-mode "aaa C-c '" code-with-comment (comment-edit-test--append-to-code-block
                                                                                       'python-mode code-with-comment "aaa"))))

(ert-deftest comment-edit-test-rb-in-rb ()
  (let ((code-with-comment
         (comment-edit-test--indent-rb
          "def sum(*nums):
             nums.inject(0) {|sum,x| sum + x }
           # ```ruby
           # sum 1, 2, 3 # <|>
           # # => 6
           # ```"))
        (code-in-comment
         (comment-edit-test--indent-sh
          "sum 1, 2, 3 # <|>
           # => 6")))
    (comment-edit-test--execute-block-edit 'ruby-mode ""          code-with-comment code-in-comment)
    (comment-edit-test--execute-block-edit 'ruby-mode "C-c '"     code-with-comment code-with-comment)
    (comment-edit-test--execute-block-edit 'ruby-mode "aaa C-c '" code-with-comment (comment-edit-test--append-to-code-block
                                                                                       'ruby-mode code-with-comment "aaa"))))

(ert-deftest comment-edit-test-sh-in-c1 ()
  (let ((code-with-comment
         (comment-edit-test--indent-c
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
         (comment-edit-test--indent-sh
          "# build <|>
           make -k")))
    (comment-edit-test--execute-block-edit 'c-mode ""           code-with-comment code-in-comment)
    (comment-edit-test--execute-block-edit 'c-mode "C-c '"      code-with-comment code-with-comment)
    (comment-edit-test--execute-block-edit 'c-mode "aaa C-c '"  code-with-comment (comment-edit-test--append-to-code-block
                                                                                  'c-mode code-with-comment "aaa"))))

(ert-deftest comment-edit-test-sh-in-c2 ()
  (let ((code-with-comment
         (comment-edit-test--indent-c
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
         (comment-edit-test--indent-sh
          "# build <|>
           make -k")))
    (comment-edit-test--execute-block-edit 'c-mode ""           code-with-comment code-in-comment)
    (comment-edit-test--execute-block-edit 'c-mode "C-c '"      code-with-comment code-with-comment)
    (comment-edit-test--execute-block-edit 'c-mode "aaa C-c '"  code-with-comment (comment-edit-test--append-to-code-block
                                                                                  'c-mode code-with-comment "aaa"))))

(ert-deftest comment-edit-test-code-in-doc-1 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ```elisp
    (hello \\\"foo\\\") ;; <|>
    ```\"
  (message \"hello, %s\" name))")
        (code-in-doc "(hello \"foo\") ;; <|>"))
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode ""       init-data code-in-doc)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "C-c '"  init-data init-data)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c '"  init-data (comment-edit-test--append-to-code-block
                                                                                           'emacs-lisp-mode init-data "aaa"))))

(ert-deftest comment-edit-test-code-in-doc-2 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ,---elisp
    | (hello \\\"foo\\\") ;; <|>
    `---\"
  (message \"hello, %s\" name))")
        (code-in-doc "(hello \"foo\") ;; <|>"))
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode ""       init-data code-in-doc)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "C-c '"  init-data init-data)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c '"  init-data (comment-edit-test--append-to-code-block
                                                                                           'emacs-lisp-mode init-data "aaa"))))

(ert-deftest comment-edit-test-code-in-doc-3 ()
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
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode ""       init-data code-in-doc)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "C-c '"  init-data init-data)
    (comment-edit-test--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c '"  init-data (comment-edit-test--append-to-code-block
                                                                                       'emacs-lisp-mode init-data "aaa"))))

(ert-deftest comment-edit-test-string-escape-js ()
  (let* ((initial-string "'\"single quotes wrap<|> double quotes.\"'")
         (expected-string (substring initial-string 1 (- (length initial-string) 1))))
    (comment-edit-test--execute-block-edit 'javascript-mode ""       initial-string expected-string)
    (comment-edit-test--execute-block-edit 'javascript-mode "C-c '"  initial-string initial-string))
  (let* ((initial-string "'\\'single quotes wrap<|> single quotes.\\''")
         (expected-string   "'single quotes wrap<|> single quotes.'"))
    (comment-edit-test--execute-block-edit 'javascript-mode ""       initial-string expected-string)
    (comment-edit-test--execute-block-edit 'javascript-mode "C-c '"  initial-string initial-string))
(let* ((initial-string "\"'double quotes wrap<|> single quotes.'\"")
       (expected-string  "'double quotes wrap<|> single quotes.'"))
    (comment-edit-test--execute-block-edit 'javascript-mode ""       initial-string expected-string)
    (comment-edit-test--execute-block-edit 'javascript-mode "C-c '"  initial-string initial-string)))

(provide 'comment-edit-test)

;;; comment-edit-test.el ends here
