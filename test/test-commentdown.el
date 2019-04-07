;;; test-commentdown.el --- Test commentdown -*- lexical-binding: t; -*-

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

(require 'commentdown)
;; (commentdown-toggle-debug t)

(when noninteractive
  (transient-mark-mode))

;;; Function test

(ert-deftest test-commentdown-comment-starter-regexp-el ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (commentdown--comment-starter-regexp 'emacs-lisp-mode) "" (car it)))))
   '((";foo"     . "foo")
     ("; foo"    . "foo")
     (";  foo"   . " foo")
     
     (";;foo"    . "foo")
     (";; foo"   . "foo")
     (";;  foo"  . " foo")
     
     (";;;foo"   . "foo")
     (";;; foo"  . "foo")
     (";;;  foo" . " foo"))))

(ert-deftest test-commentdown-comment-starter-regexp-py ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (commentdown--comment-starter-regexp 'python-mode) "" (car it)))))
   '(("#foo"     . "foo")
     ("# foo"    . "foo")
     ("#  foo"   . " foo")
     
     ("##foo"    . "foo")
     ("## foo"   . "foo")
     ("##  foo"  . " foo")
     
     ("###foo"   . "foo")
     ("### foo"  . "foo")
     ("###  foo" . " foo"))))

(ert-deftest test-commentdown-comment-starter-regexp-c1 ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (commentdown--comment-starter-regexp 'c-mode) "" (car it)))))
   '(("*foo"     . "foo")
     ("* foo"    . "foo")
     ("*  foo"   . " foo")

     ("**foo"    . "foo")
     ("** foo"   . "foo")
     ("**  foo"  . " foo")

     ("***foo"   . "foo")
     ("*** foo"  . "foo")
     ("***  foo" . " foo"))))

(ert-deftest test-commentdown-comment-starter-regexp-c2 ()
  (mapc
   (lambda (it)
     (should
      (equal (cdr it)
             (replace-regexp-in-string
              (commentdown--comment-starter-regexp 'c-mode) "" (car it)))))
   '(("/foo"     . "/foo")
     ("/ foo"    . "/ foo")
     ("/  foo"   . "/  foo")

     ("//foo"    . "foo")
     ("// foo"   . "foo")
     ("//  foo"  . " foo")

     ("///foo"   . "foo")
     ("/// foo"  . "foo")
     ("///  foo" . " foo"))))

;;; Interaction test

(ert-deftest test-commentdown-el-in-el ()
  (let ((code-with-comment
         (test-commentdown--indent-el
          "(defun sum (&rest nums)
             (funcall '+ nums))
           ;; ```elisp
           ;; (sum '(1 2 3)) ;; <|>
           ;; ;; => 6
           ;;```"))
        (code-in-editing
         (test-commentdown--indent-el
          "(sum '(1 2 3)) ;; <|>
           ;; => 6")))
    (test-commentdown--execute-block-edit 'emacs-lisp-mode ""           code-with-comment code-in-editing)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "C-c '"      code-with-comment code-with-comment)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "aaa C-c '"  code-with-comment (test-commentdown--append-to-code-block
                                                                                           'emacs-lisp-mode code-with-comment "aaa"))))

(ert-deftest test-commentdown-py-in-py ()
  (let ((code-with-comment
         (test-commentdown--indent-py
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
         (test-commentdown--indent-py
          "sum(1, 2, 3) # <|>
           # => 6")))
    (test-commentdown--execute-block-edit 'python-mode ""          code-with-comment code-in-comment)
    (test-commentdown--execute-block-edit 'python-mode "C-c '"     code-with-comment code-with-comment)
    (test-commentdown--execute-block-edit 'python-mode "aaa C-c '" code-with-comment (test-commentdown--append-to-code-block
                                                                                       'python-mode code-with-comment "aaa"))))

(ert-deftest test-commentdown-rb-in-rb ()
  (let ((code-with-comment
         (test-commentdown--indent-rb
          "def sum(*nums):
             nums.inject(0) {|sum,x| sum + x }
           # ```ruby
           # sum 1, 2, 3 # <|>
           # # => 6
           # ```"))
        (code-in-comment
         (test-commentdown--indent-sh
          "sum 1, 2, 3 # <|>
           # => 6")))
    (test-commentdown--execute-block-edit 'ruby-mode ""          code-with-comment code-in-comment)
    (test-commentdown--execute-block-edit 'ruby-mode "C-c '"     code-with-comment code-with-comment)
    (test-commentdown--execute-block-edit 'ruby-mode "aaa C-c '" code-with-comment (test-commentdown--append-to-code-block
                                                                                       'ruby-mode code-with-comment "aaa"))))

(ert-deftest test-commentdown-sh-in-c1 ()
  (let ((code-with-comment
         (test-commentdown--indent-c
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
         (test-commentdown--indent-sh
          "# build <|>
           make -k")))
    (test-commentdown--execute-block-edit 'c-mode ""           code-with-comment code-in-comment)
    (test-commentdown--execute-block-edit 'c-mode "C-c '"      code-with-comment code-with-comment)
    (test-commentdown--execute-block-edit 'c-mode "aaa C-c '"  code-with-comment (test-commentdown--append-to-code-block
                                                                                  'c-mode code-with-comment "aaa"))))

(ert-deftest test-commentdown-sh-in-c2 ()
  (let ((code-with-comment
         (test-commentdown--indent-c
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
         (test-commentdown--indent-sh
          "# build <|>
           make -k")))
    (test-commentdown--execute-block-edit 'c-mode ""           code-with-comment code-in-comment)
    (test-commentdown--execute-block-edit 'c-mode "C-c '"      code-with-comment code-with-comment)
    (test-commentdown--execute-block-edit 'c-mode "aaa C-c '"  code-with-comment (test-commentdown--append-to-code-block
                                                                                  'c-mode code-with-comment "aaa"))))

(ert-deftest test-commentdown-code-in-doc-1 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ```elisp
    (hello \\\"foo\\\") ;; <|>
    ```\"
  (message \"hello, %s\" name))")
        (code-in-doc "(hello \"foo\") ;; <|>"))
    (test-commentdown--execute-block-edit 'emacs-lisp-mode ""       init-data code-in-doc)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "C-c '"  init-data init-data)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c '"  init-data (test-commentdown--append-to-code-block
                                                                                           'emacs-lisp-mode init-data "aaa"))))

(ert-deftest test-commentdown-code-in-doc-2 ()
  (let ((init-data "(defun hello (name)
  \"Greet a person.

Usage:

    ,---elisp
    | (hello \\\"foo\\\") ;; <|>
    `---\"
  (message \"hello, %s\" name))")
        (code-in-doc "(hello \"foo\") ;; <|>"))
    (test-commentdown--execute-block-edit 'emacs-lisp-mode ""       init-data code-in-doc)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "C-c '"  init-data init-data)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c '"  init-data (test-commentdown--append-to-code-block
                                                                                           'emacs-lisp-mode init-data "aaa"))))

(ert-deftest test-commentdown-code-in-doc-3 ()
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
    (test-commentdown--execute-block-edit 'emacs-lisp-mode ""       init-data code-in-doc)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "C-c '"  init-data init-data)
    (test-commentdown--execute-block-edit 'emacs-lisp-mode "M-> aaa C-c '"  init-data (test-commentdown--append-to-code-block
                                                                                       'emacs-lisp-mode init-data "aaa"))))

(provide 'test-commentdown)

;;; test-commentdown.el ends here
