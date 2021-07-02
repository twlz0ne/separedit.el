;;; test-checkinstall.el ---  -*- lexical-binding: t; -*-

;; Checks & Install packages declared in Cask file

(toggle-debug-on-error)
(message "==> user-emacs-directory: %s" user-emacs-directory)
(message "==> package-user-dir: %s" package-user-dir)

(defvar test-checkinstall-selected-packages nil)
(defvar test-checkinstall-selected-sources nil)

(defconst test-checkinstall-source-alist
  (if (load "~/.emacs.d/elpa.el" t)
      package-archives
    '(("gnu"          . "http://elpa.gnu.org/packages/")
      ("melpa"        . "http://melpa.org/packages/")
      ("melpa-Stable" . "http://stable.melpa.org/packages/")
      ("marmalade"    . "http://marmalade-repo.org/packages/")
      ("org"          . "http://orgmode.org/elpa/"))))

(defun ensure-quelpa ()
  (message "==> ensure-quelpa")
  (setq quelpa-dir (expand-file-name ".cask/quelpa"))
  (unless (file-exists-p quelpa-dir)
    (make-directory quelpa-dir))
  (unless (package-installed-p 'quelpa)
    (package-install 'quelpa))
  (require 'quelpa)
  (quelpa-checkout-melpa)
  (setq quelpa-update-melpa-p nil)
  (require 'subr-x)
  (when-let ((proxy (getenv "PROXY")))
    (advice-add 'quelpa :around
                `(lambda (fn arg &rest plist)
                   (with-temp-buffer
                     (let ((process-environment (cl-copy-list process-environment))
                           (url-proxy-services
                            '(("http"     . ,proxy)
                              ("https"    . ,proxy)
                              ("ftp"      . ,proxy)
                              ("no_proxy" . ,(concat "^"
                                                     "\\(localhost"
                                                     "\\|127.0.0.1"
                                                     "\\|192.168.*"
                                                     "\\|10.*\\)")))))
                       (setenv "http_proxy" ,proxy)
                       (setenv "https_proxy" ,proxy)
                       (message "Proxy: t")
                       (apply fn arg plist)))))))



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

(defmacro depends-on (package-name &rest args)
  (let ((msg (format "==> check package `%s'" package-name))
        (pkg (intern package-name)))
    `(condition-case err
         (progn
           (require ',pkg)
           (message "%-50s [ ✓ ]" ,msg))
       (error
        (message "%-50s [ ✗ ]" ,msg)
        (add-to-list 'test-checkinstall-selected-packages '(,pkg ,@args))))))



(message "==> Start checkinstall...")
(load (expand-file-name "Cask"))

(when test-checkinstall-selected-packages
  (message "==> cask install ...")
  (setq package-archives test-checkinstall-selected-sources)
  (message "==> package-archives: %s" package-archives)

  ;; FIX: Failed to download 'xxx' archive
  (when (and noninteractive (<= 25.1 (string-to-number emacs-version) 26.1))
    (require 'gnutls)
    (with-eval-after-load 'gnutls
      (message "==> downgrade network security level to 'low")
      (setq network-security-level 'low)))

  ;; Fix: bad-signature "archive-contents.sig"
  ;; https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  (when (and noninteractive (< (string-to-number emacs-version) 26.1))
    (let ((gnupg-dir (expand-file-name "gnupg" package-user-dir)))
      (message "==> update gpg key")
      (message
       (shell-command-to-string
        (format "mkdir -p %s && gpg --homedir %s --receive-keys 066DAFCB81E42C40"
                gnupg-dir gnupg-dir)))

      ;; Fix gpg error:
      ;; $ gpg --homedir %s --receive-keys 066DAFCB81E42C40
      ;; gpg: key 066DAFCB81E42C40: new key but contains no user ID - skipped
      ;; gpg: Total number processed: 1
      ;; gpg:           w/o user IDs: 1
      (let* ((default-directory (expand-file-name ".cask/" user-emacs-directory))
             (tar "gnu-elpa-keyring-update-2019.3.tar"))
        (unless (file-exists-p tar)
          (message "==> download %s" tar)
          (url-copy-file (concat "https://elpa.gnu.org/packages/" tar) tar))
        (unless (featurep 'gnu-elpa-keyring-update)
          (with-current-buffer (find-file-noselect tar)
            (message "==> install %s" tar)
            (package-install-from-buffer)))
        (gnu-elpa-keyring-update))))

  (message "==> package-refresh-contents")
  (package-refresh-contents)

  (mapc
   (pcase-lambda (`(,pkg . ,args))
     (message "==> installing %s..." pkg)
     (when (stringp (car args))
       (pop args)) ;; FIXME
     (if args
         (let* ((url)
                (fetcher
                 (catch 'break
                   (dolist (key '(:bzr :cvs :darcs :git :hg :svn))
                     (when (setq url (plist-get args key))
                       (throw 'break (substring (symbol-name key) 1)))))))
           (when fetcher
             (unless (featurep 'quelpa)
               (ensure-quelpa))
             (quelpa (list pkg
                           :url url
                           :fetcher (make-symbol fetcher)
                           :commit (plist-get args :ref)
                           :branch (plist-get args :branch)
                           :files (plist-get args :files)))))
       (package-install pkg))
     (add-to-list 'package-selected-packages pkg))
   (reverse test-checkinstall-selected-packages)))

;;; test-checkinstall.el ends here
