;;----------------------------------------------------------------------
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
;; (setq package-list '(helm
;;                      auctex
;;                      ess
;;                      ess-view
;;                      polymode
;;                      auto-complete
;;                      company
;;                      bm
;;                      smartparens
;;                      yafolding
;;                      magit
;;                      markdown-mode
;;                      markdown-toc
;;                      neotree
;;                      imenu-list
;;                      key-combo
;;                      elpy
;;                      jedi
;;                      anaconda-mode
;;                      web-mode
;;                      monokai-theme
;;                      molokai-theme
;;                      solarized-theme
;;                      gotham-theme
;;                      leuven-theme
;;                      flatui-theme
;;                      spacemacs-theme))
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
(package-install 'auctex)
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
(package-install 'imenu-list)
;; (package-install 'sublimity)
(package-install 'key-combo)
(package-install 'elpy)
(package-install 'jedi)
(package-install 'anaconda-mode)
;; (package-install 'jedi-core)
(package-install 'web-mode)

;; Light.
(package-install 'monokai-theme)
(package-install 'molokai-theme)
(package-install 'gotham-theme)
;; Dark.
(package-install 'leuven-theme)
(package-install 'flatui-theme)
;; Both.
(package-install 'solarized-theme)
(package-install 'spacemacs-theme)

;; NOTE: after install `jedi` and `jedy-core`, run `M-x
;; jedi:install-server` in a new GNU Emacs session to enable proper auto
;; completation for Python scripts.

;; (byte-recompile-directory "~/.emacs.d/elpa/bookmark+" 0 t)

;; NOTE: install `sudo apt-get install python3-pip` is important to Elpy
;; works properly may due some things that are installed along side with
;; pip3.

;;----------------------------------------------------------------------
