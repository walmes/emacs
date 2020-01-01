;;----------------------------------------------------------------------
;; ATTENTION: Run it in terminal in batch mode.
;;   emacs --script update-packages.el

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(setq package-check-signature nil)

(package-list-packages)
(package-refresh-contents)

(package-menu-mark-upgrades)
(package-menu-execute)

;;----------------------------------------------------------------------
