;; ATTENTION: Run it in terminal in batch mode.
;;   emacs --script install-packages.el

(require 'package)
(package-initialize)

;; (add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-list-packages)

;;----------------------------------------------------------------------
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

;; ;; List the packages you want.
;; (setq package-list '(ess-view
;;                      leuven-theme
;;                      gotham-theme
;;                      molokai-theme
;;                      solarized-theme
;;                      key-combo))
;;
;; ;; Activate all the packages (in particular autoloads).
;; (package-initialize)
;;
;; ;; Fetch the list of packages available.
;; (unless package-archive-contents
;;   (package-refresh-contents))
;;
;; ;; Install the missing packages.
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;;----------------------------------------------------------------------

;; To install a package.
;; (when (not (package-installed-p 'smartparens))
;;  (package-install 'smartparens))

;; Runs the installation of each package.
(package-install 'helm)
(package-install 'ess)
(package-install 'ess-view)
(package-install 'polymode)
;; (package-install 'ess-R-data-view)
(package-install 'auto-complete)
(package-install 'company)
(package-install 'bm)
;; (package-install 'bookmark)
(package-install 'smartparens)
(package-install 'yafolding)
(package-install 'magit)
(package-install 'markdown-mode)
(package-install 'markdown-toc)
(package-install 'neotree)
;; (package-install 'sublimity)
(package-install 'key-combo)
(package-install 'elpy)
(package-install 'jedi)
;; (package-install 'jedi-core)

(package-install 'monokai-theme)
(package-install 'molokai-theme)
(package-install 'solarized-theme)
(package-install 'gotham-theme)
(package-install 'leuven-theme)

;; NOTE: after install `jedi` and `jedy-core`, run `M-x
;; jedi:install-server` in a new GNU Emacs session to enable proper auto
;; completation for Python scripts.

;; (byte-recompile-directory "~/.emacs.d/elpa/bookmark+" 0 t)

;;----------------------------------------------------------------------
