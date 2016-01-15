;;======================================================================
;; Configuration file to Emacs (>=24.3.1) by Walmes Zeviani.
;;
;; This file is hosted at https://github.com/walmes/emacs2.
;;
;; Almost all the content available here was obtained/inspired by
;; queries on the internet. Please, send questions, problems and/or
;; suggestions as an issue on GitHub directory of this file.
;;======================================================================

;; http://www.emacswiki.org/wiki/EmacsNiftyTricks
;; “I’ve used Emacs for many years now, but have never reached its
;;    maximum potential.” – Anon.
;;
;; http://www.mygooglest.com/fni/dot-emacs.html
;; “Show me your ~/.emacs and I will tell
;;    you who you are.” - Bogdan Maryniuk.

;;----------------------------------------------------------------------
;; Basic definitions.
;;----------------------------------------------------------------------

;; Add directory with supplementary configuration files.
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; Highlight the cursor line.
(global-hl-line-mode 1)

;; Allows delete selected text region.
(delete-selection-mode 1)

;; Cursor position.
(setq column-number-mode t)

;; Highlight matching pairs.
(show-paren-mode 1)

;; Auto break line at 72 characters.
(setq-default fill-column 72)

;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
;; (require 'whitespace)
;(setq whitespace-line-column fill-column)
;(setq whitespace-style
;      '(face lines-tail trailing spaces tabs empty))
;(global-whitespace-mode +1)

;; Activate auto-fill-mode to make auto break lines.
;(setq-default auto-fill-function 'do-auto-fill)

;; Screen lines instead of logical lines.
(visual-line-mode 1)

;; Allow shared transfer area.
(setq x-select-enable-clipboard t)

;; Comment even in empty lines.
(setq comment-empty-lines t)

;; Remove white espace at end when save buffer.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turn off auto save and auto backup.
(setq auto-save-default nil) ;; Para o #autosave#.
(setq make-backup-files nil) ;; Para o backup~.
