(require 'test-bootstrap)

(toggle-debug-on-error)
(message "==> user-emacs-directory: %s" user-emacs-directory)
(message "==> package-user-dir: %s" package-user-dir)

(defvar test-checkinstall-selected-packages nil)
(defvar test-checkinstall-selected-sources nil)

(defconst test-checkinstall-source-alist
  '(("gnu"          . "http://elpa.gnu.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-Stable" . "http://stable.melpa.org/packages/")
    ("marmalade"    . "http://marmalade-repo.org/packages/")
    ("org"          . "http://orgmode.org/elpa/")))

(defmacro source (name)
  `(let ((it (assoc ,(format "%s" name) test-checkinstall-source-alist)))
     (add-to-list 'test-checkinstall-selected-sources it)))

(defmacro package-file (&rest args)
  `(progn
     ,@args))

(defmacro files (&rest args)
  `(progn
     ,@args))

(defmacro development (&rest args)
  `(progn
     ,@args))

(defmacro depends-on (package-name)
  `(let ((msg ,(format "==> check package `%s'" package-name))
         (pkg ',(intern package-name)))
    (condition-case err
      (progn
        (require pkg)
        (message "%-50s [ ✓ ]" msg))
    (error
     (message "%-50s [ ✗ ]" msg)
     (add-to-list 'test-checkinstall-selected-packages pkg)))))

(message "==> Start checkinstall...")
(load (expand-file-name "Cask"))

(when test-checkinstall-selected-packages
  (message "==> cask install ...")
  (setq package-selected-packages (reverse test-checkinstall-selected-packages))
  (setq package-archives test-checkinstall-selected-sources)
  (message "==> selected-packages: %s" package-selected-packages)
  (message "==> package-archives: %s" package-archives)

  ;; Fix: bad-signature "archive-contents.sig"
  ;; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  (let ((gnupg-dir (expand-file-name "gnupg" package-user-dir)))
    (message
     (shell-command-to-string
      (format "mkdir -p %s && gpg --homedir %s --receive-keys 066DAFCB81E42C40"
              gnupg-dir gnupg-dir))))

  ;; ;; FIX: Failed to download 'xxx' archive
  ;; (when (and noninteractive (<= 25.1 (string-to-number emacs-version) 26.1))
  ;;   (require 'gnutls)
  ;;   (with-eval-after-load 'gnutls
  ;;     (message "==> downgrade network security level")
  ;;     (setq network-security-level 'low)))

  ;; (package-initialize)
  (package-refresh-contents)

  (mapc (lambda (pkg)
          (message "==> installing %s..." pkg)
          (package-install pkg))
        package-selected-packages))


