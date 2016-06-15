;; ATTENTION: Run it in terminal in batch mode.
;;   emacs -batch -l install-packages.el

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(package-list-packages)

;; To install a package.
;; (when (not (package-installed-p 'smartparens))
;;  (package-install 'smartparens))

;; List of packages.
(defvar prelude-packages
  "A list of packages to ensure are installed at launch."
  '(helm
    auto-complete
    company
    auctex
    bm
    bookmark+
    color-theme color-theme-solarized
    ess ess-R-data-view ess-R-object-popup
    leuven-theme
    magit magit-popup
    markdown-mode markdown-toc
    polymode
    smartparens
    yafolding))

;; Loop that check presence and install.
(cl-loop
 for p in prelude-packages
 when (not (package-installed-p p))
 do (package-install p))
