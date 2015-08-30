;;======================================================================
;; This file makes installation of emacs packages in the melpa
;; repository. You must run it in Linux terminal by:
;; 
;; $ emacs --batch -l library-install.el
;;
;; All the packages listed bellow will be installed. The commented
;; packages are dependencies from the non commented ones. ATTENTION! It
;; assumes Emacs >=24.4, althought was testet only with Emacs 24.5.2.
;; 
;; Inspired by:
;; http://hacks-galore.org/aleix/blog/archives/2013/01/08/install-emacs-packages-from-command-line
;; http://www.emacswiki.org/emacs/BatchMode
;;======================================================================

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(package-install 'auctex)
(package-install 'auto-complete)
(package-install 'bm)
(package-install 'bookmark+)
(package-install 'color-theme)
(package-install 'color-theme-solarized)
(package-install 'ess)
(package-install 'ess-R-data-view)
(package-install 'ess-R-object-popup)
(package-install 'leuven-theme)
(package-install 'magit)
(package-install 'magit-popup)
(package-install 'markdown-mode)
(package-install 'markdown-toc)
(package-install 'polymode)
(package-install 'yafolding)
;(package-install 'async)
;(package-install 'ctable)
;(package-install 'dash)
;(package-install 'git-commit)
;(package-install 'julia-mode)
;(package-install 'popup)
;(package-install 'with-editor)
