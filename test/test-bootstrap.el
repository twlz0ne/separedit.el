;;; test-bootstrap.el ---  -*- lexical-binding: t; -*-

;; Just import packages downloaded by cask,
;; make Makefile free from cask-cli -- it's too slow.

(let ((ver-list (version-to-list emacs-version)))
  (setq user-emacs-directory (expand-file-name "./"))
  (setq package-user-dir (expand-file-name (format "./.cask/%s.%s/elpa/" (car ver-list) (car (cdr ver-list)))))
  (package-initialize))

(if (version-list-<= '(25 1) (list emacs-major-version emacs-minor-version))
    (define-advice display-warning (:around (fn type message &rest rest) ignore-load-path-warning)
        "Ignore the warning about the `.emacs.d' directory being in `load-path'."
        (unless (and (eq type 'initialization)
                     (string-prefix-p "Your ‘load-path’ seems to contain\nyour ‘.emacs.d’ directory"
                                      message
                                      t))
          (apply fn type message rest))))

;;; test-bootstrap.el ends here
