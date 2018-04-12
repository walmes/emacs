;; ATTENTION: Run it in terminal in batch mode.
;;   emacs -batch -l install-packages.el
;; UPDATE: it does not work as expected.  Is is better run each package-install.

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(package-list-packages)

;; To install a package.
;; (when (not (package-installed-p 'smartparens))
;;  (package-install 'smartparens))

;; ;; List of packages.
;; (defvar prelude-packages
;;   "A list of packages to ensure are installed at launch."
;;   '(helm
;;     auto-complete
;;     company
;;     auctex
;;     bm
;;     bookmark+
;;     color-theme color-theme-solarized
;;     ess ess-R-data-view ess-R-object-popup
;;     magit magit-popup
;;     markdown-mode markdown-toc
;;     polymode
;;     smartparens
;;     yafolding))

;; ;; Loop that check presence and install.
;; (cl-loop
;;  for p in prelude-packages
;;  when (not (package-installed-p p))
;;  do (package-install p))

;; Runs the installation of each package.
(package-install 'helm)
(package-install 'auto-complete)
(package-install 'company)
(package-install 'bm)
(package-install 'bookmark)
;; (package-install 'color-theme)
;; (package-install 'color-theme-solarized)
(package-install 'monokai-theme)
(package-install 'smartparens)
(package-install 'yafolding)
(package-install 'ess)
(package-install 'ess-R-data-view)
(package-install 'elpy)
(package-install 'magit)
(package-install 'markdown-mode)
(package-install 'markdown-toc)
(package-install 'polymode)
