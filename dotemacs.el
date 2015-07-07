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
(add-to-list 'load-path "~/.emacs.d/")

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

;; Activate auto-fill-mode to make auto break lines.
(setq-default auto-fill-function 'do-auto-fill)

;; Screen lines instead of logical lines.
(visual-line-mode 1)
 
;; Allow shared transfer area.
(setq x-select-enable-clipboard t)

;; Comment even in empty lines.
(setq comment-empty-lines t)

;; Turn off auto save and auto backup.
(setq auto-save-default nil) ;; Para o #autosave#.
(setq make-backup-files nil) ;; Para o backup~.

;; Spaces to indent.
;; http://xenon.stanford.edu/~manku/dotemacs.html
(setq-default indent-tabs-mode nil)

;; Font and size.
(set-default-font "Ubuntu Mono-13")

;; Turn ido-mode on.
;; http://www.emacswiki.org/InteractivelyDoThings
(ido-mode t)

;; To work the accents on Sony Vaio.
(require 'iso-transl)

;; Open Emacs without start-up screen.
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]

;;----------------------------------------------------------------------
;; Key bindings.
;;----------------------------------------------------------------------

;; C-TAB to move the cursor between visible buffers.
(global-set-key [(control tab)] 'other-window)

;; C-page down e C-page up to move along buffers.
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)

;; C-z to 'undo, the default is C-/.
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; M-. to (un)comment paragraph.
(global-set-key [?\M-.] (kbd "M-h M-; M-}"))

;; M-+ to indent paragraph.
(global-set-key [?\M-+] (kbd "M-h C-M-\\"))

;; M-= to get division line with 70 =.
(global-set-key [?\M-=] (kbd "C-u 7 0 ="))

;; M-- to get division line with 70 -.
(global-set-key [?\M--] (kbd "C-u 7 0 -"))

;; C-- to get division line with 43 -.
(global-set-key [?\C--] (kbd "C-u 4 3 -"))

;; S-F11 and S-F12 to show/hide menu bar and tool bar.
(global-set-key (kbd "<S-f11>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<S-f12>") 'toggle-tool-bar-mode-from-frame)

;;----------------------------------------------------------------------
;; Functions.
;;----------------------------------------------------------------------

(load "~/.emacs.d/functions")

;;----------------------------------------------------------------------
;; Extensions.
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Solarized color theme.

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(add-to-list 'load-path
             "~/.emacs.d/emacs-color-theme-solarized")
(require 'color-theme-solarized)
;; (color-theme-solarized-dark)
;; (color-theme-solarized-light)

;;----------------------------------------------------------------------
;; MarkDown extensions.
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

(setq load-path
      (append '("~/.emacs.d/markdown-mode")
              load-path))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;----------------------------------------------------------------------
;; R+MarkDown extensions (emacs >= 24.3.1).
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

(setq load-path
      (append '("~/.emacs.d/polymode/"
                "~/.emacs.d/polymode/modes")
              load-path))

(require 'poly-R)
(require 'poly-markdown)
(require 'poly-noweb)

(autoload 'poly-markdown-mode "poly-markdown-mode"
  "Major mode for editing R-Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))

;; Insert a new (empty) chunk to R markdown.
(defun insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r}\n\n```")
  (forward-line -1)
  )

(global-set-key (kbd "C-c i") 'insert-chunk)

;;----------------------------------------------------------------------
;; Key bindings for R, Rnw and Rmd sessions.
;; http://stackoverflow.com/questions/2901198/useful-keyboard-shortcuts-and-tips-for-ess-r

;; Movement across chunks in Rnw files.
(global-set-key (kbd "C-S-<f5>") 'ess-eval-chunk)
(global-set-key (kbd "C-S-<f6>") 'ess-eval-chunk-and-step)
(global-set-key (kbd "C-S-<f7>") 'ess-noweb-next-code-chunk)
(global-set-key (kbd "C-S-<f8>") 'ess-noweb-previous-code-chunk)
(global-set-key (kbd "C-S-<f9>") 'ess-noweb-goto-chunk)

;; Movement across chunks in Rmd files.
(global-set-key (kbd "S-<f7>") 'polymode-previous-chunk-same-type)
(global-set-key (kbd "S-<f8>") 'polymode-next-chunk-same-type)
(global-set-key (kbd "S-<f9>") 'polymode-insert-new-chunk)

;; Insert a new (empty) chunk to R markdown.
(defun insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r}\n\n```")
  (forward-line -1)
  )

(add-hook 'poly-markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c i") 'insert-chunk)))

;;----------------------------------------------------------------------
;; To eval line/regions in terminal open in Emacs.

(require 'essh)

(global-set-key [?\C-*] 'pipe-line-to-shell-and-step)
(global-set-key [?\C-&] 'pipe-region-to-shell)

;;----------------------------------------------------------------------
;; Auto complete mode for Emacs.

(add-to-list 'load-path
             "~/.emacs.d/auto-complete/")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/auto-complete/dict/")
(ac-config-default)

(setq ess-use-auto-complete 'script-only)

;; Change 'ac-complete from ENTER to TAB.
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map "\t" 'ac-complete)

;;----------------------------------------------------------------------
;; Bookmark-plus.

;; (setq bookmark-default-file "~/.emacs.d/bookmarks")
(setq
 bookmark-default-file "~/Dropbox/bookmarks"
 bookmark-save-flag 1)

(add-to-list 'load-path
             "~/.emacs.d/bookmark-plus/")
(require 'bookmark+)

;; Temporary bookmarks.
;; (bmkp-toggle-autotemp-on-set)

;;----------------------------------------------------------------------
;; Visible bookmarks. Easy movement.
;; https://marmalade-repo.org/packages/bm

(add-to-list 'load-path "~/.emacs.d/bm/")
(require 'bm)

;; http://emacsworld.blogspot.com.br/2008/09/visual-bookmarks-package-for-emacs.html
;; Customize the colors by using M-x customize-group RET bm RET
(setq bm-marker 'bm-marker-left)
(setq bm-highlight-style 'bm-highlight-only-fringe)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;;----------------------------------------------------------------------
;; Latex extensions.

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(require 'tex-site)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Open Tikz files (pgf and pgs extensions) in Tex mode.
(add-to-list 'auto-mode-alist '("\\.pgf" . tex-mode))
(add-to-list 'auto-mode-alist '("\\.pgs" . tex-mode))

;;----------------------------------------------------------------------
