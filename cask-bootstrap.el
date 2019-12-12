;; Just import packages downloaded by cask,
;; make Makefile free from cask-cli -- it's too slow.
(let ((ver-list (version-to-list emacs-version)))
  (setq user-emacs-directory (expand-file-name "./"))
  (setq package-user-dir (expand-file-name (format "./.cask/%s.%s/elpa/" (car ver-list) (car (cdr ver-list)))))
  (package-initialize))

(provide 'cask-bootstrap)
