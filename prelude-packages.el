;; The content below was taken at
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

;; I only replace the package list. For more and more files, visit
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el

(defvar prelude-packages
  '(async
    auctex
    auto-complete
    bm
    bookmark+
    color-theme
    color-theme-solarized
    ctable
    dash
    ess
    ess-R-data-view
    ess-R-object-popup
    git-commit
    julia-mode
    leuven-theme
    magit
    magit-popup
    markdown-mode
    markdown-toc
    polymode
    popup
    with-editor
    yafolding)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; Check for new packages (package versions).
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages.
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)


